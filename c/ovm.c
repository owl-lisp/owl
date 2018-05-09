/* Owl Lisp runtime */

#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <inttypes.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <string.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <termios.h>
#include <stdio.h>

#ifndef EMULTIHOP
#define EMULTIHOP -1
#endif
#ifndef ENODATA
#define ENODATA -1
#endif
#ifndef ENOLINK
#define ENOLINK -1
#endif
#ifndef ENOSR
#define ENOSR -1
#endif
#ifndef ENOSTR
#define ENOSTR -1
#endif
#ifndef ETIME
#define ETIME -1
#endif
#ifndef F_DUPFD_CLOEXEC
#define F_DUPFD_CLOEXEC -1
#endif
#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif
#ifndef O_EXEC
#define O_EXEC 0
#endif
#ifndef O_NOFOLLOW
#define O_NOFOLLOW 0
#endif
#ifndef O_RSYNC
#define O_RSYNC 0
#endif
#ifndef O_SEARCH
#define O_SEARCH 0
#endif
#ifndef O_TTY_INIT
#define O_TTY_INIT 0
#endif
#ifdef __APPLE__
#define st_atim st_atimespec
#define st_mtim st_mtimespec
#define st_ctim st_ctimespec
#endif

typedef uintptr_t word;
typedef uint8_t   byte;
typedef intptr_t wdiff;

/*** Macros ***/

#define IPOS                        8 /* offset of immediate payload */
#define SPOS                        16 /* offset of size bits in header immediate values */
#define TPOS                        2  /* offset of type bits in header */
#define V(ob)                       (*(word *)(ob))
#define W                           ((unsigned int)sizeof(word))
#define NWORDS                      1024*1024*8    /* static malloc'd heap size if used as a library */
#define FBITS                       24             /* bits in fixnum, on the way to 24 and beyond */
#define FMAX                        ((1<<FBITS)-1) /* maximum fixnum (and most negative fixnum) */
#define MAXOBJ                      0xffff         /* max words in tuple including header */
#define MAXPAYL                     ((MAXOBJ - 1) * W) /* maximum payload in an allocated object */
#define RAWBIT                      2048
#define OBJWORDS(bytes)             ((W + (bytes) + W - 1) / W)
#define make_immediate(value, type) (((word)(value) << IPOS) | ((type) << TPOS) | 2)
#define make_header(size, type)     (((word)(size) << SPOS) | ((type) << TPOS) | 2)
#define make_raw_header(s, t, p)    (((word)(s) << SPOS) | RAWBIT | ((p) << 8) | ((t) << TPOS) | 2)
#define BOOL(cval)                  ((cval) ? ITRUE : IFALSE)
#define immval(desc)                ((desc) >> IPOS)
#define fixnump(desc)               (((desc) & 255) == 2)
#define NR                          190 /* fixme, should be ~32, see n-registers in register.scm */
#define header(x)                   V(x)
#define imm_type(x)                 (((x) >> TPOS) & 63)
#define is_type(x, t)               (((x) & (63 << TPOS | 2)) == ((t) << TPOS | 2))
#define hdrsize(x)                  (((word)(x) >> SPOS) & MAXOBJ)
#define immediatep(x)               ((word)(x) & 2)
#define allocp(x)                   (!immediatep(x))
#define rawp(hdr)                   ((hdr) & RAWBIT)
#define NEXT(n)                     ip += n; op = *ip++; goto main_dispatch /* default NEXT, smaller vm */
#define NEXT_ALT(n)                 ip += n; op = *ip++; EXEC /* more branch predictor friendly, bigger vm */
#define PAIRHDR                     make_header(3, 1)
#define NUMHDR                      make_header(3, 40) /* <- on the way to 40, see type-int+ in defmac.scm */
#define NUMNHDR                     make_header(3, 41)
#define pairp(ob)                   (allocp(ob) && V(ob) == PAIRHDR)
#define cons(a, d)                  mkpair(PAIRHDR, a, d)
#define INULL                       make_immediate(0, 13)
#define IFALSE                      make_immediate(1, 13)
#define ITRUE                       make_immediate(2, 13)
#define IEMPTY                      make_immediate(3, 13) /* empty ff */
#define IEOF                        make_immediate(4, 13)
#define IHALT                       make_immediate(5, 13)
#define TNUM                        0
#define TTUPLE                      2
#define TSTRING                     3
#define TPORT                       12
#define TTHREAD                     31
#define TNUMN                       32
#define TFF                         24
#define FFRIGHT                     1
#define FFRED                       2
#define TBVEC                       19
#define TBYTECODE                   16
#define TPROC                       17
#define TCLOS                       18
#define F(value)                    make_immediate(value, TNUM)
#define stringp(ob)                 (allocp(ob) && (V(ob) & make_header(0, 63)) == make_header(0, TSTRING))
#define FLAG                        1
#define cont(n)                     V((word)(n) & ~FLAG)
#define flag(n)                     ((word)(n) ^ FLAG)
#define flagged(n)                  ((word)(n) & FLAG)
#define flagged_or_raw(n)           ((word)(n) & (RAWBIT | FLAG))
#define TBIT                        0x1000
#define teardown_needed(hdr)        ((word)(hdr) & TBIT)
#define A0                          R[*ip]
#define A1                          R[ip[1]]
#define A2                          R[ip[2]]
#define A3                          R[ip[3]]
#define A4                          R[ip[4]]
#define A5                          R[ip[5]]
#define G(ptr, n)                   (((word *)(ptr))[n])
#define TICKS                       10000 /* # of function calls in a thread quantum */
#define allocate(size, to)          (to = fp, fp += size)
#define error(opcode, a, b)         R[4] = F(opcode); R[5] = (word)a; R[6] = (word)b; goto invoke_mcp;
#define assert(exp, val, code)      if (!(exp)) { error(code, val, ITRUE); }
#define assert_not(exp, val, code)  if (exp) { error(code, val, ITRUE); }
#define MEMPAD                      (NR + 2) * 8 /* space at end of heap for starting GC */
#define MINGEN                      1024 * 32 /* minimum generation size before doing full GC */
#define INITCELLS                   100000
#define OCLOSE(proctype)            { word size = *ip++, tmp; word *ob; allocate(size, ob); tmp = R[*ip++]; tmp = G(tmp, *ip++); *ob = make_header(size, proctype); ob[1] = tmp; tmp = 2; while (tmp != size) ob[tmp++] = R[*ip++]; R[*ip++] = (word)ob; }
#define CLOSE1(proctype)            { word size = *ip++, tmp; word *ob; allocate(size, ob); tmp = R[1]; tmp = G(tmp, *ip++); *ob = make_header(size, proctype); ob[1] = tmp; tmp = 2; while (tmp != size) ob[tmp++] = R[*ip++]; R[*ip++] = (word)ob; }
#define EXEC switch(op&63) { \
      case 0: goto op0; case 1: goto op1; case 2: goto op2; case 3: goto op3; case 4: goto op4; case 5: goto op5; \
      case 6: goto op6; case 7: goto op7; case 8: goto op8; case 9: goto op9; \
      case 10: goto op10; case 11: goto op11; case 12: goto op12; case 13: goto op13; case 14: goto op14; case 15: goto op15; \
      case 16: goto op16; case 17: goto op17; case 18: goto op18; case 19: goto op19; case 20: goto op20; case 21: goto op21; \
      case 22: goto op22; case 23: goto op23; case 24: goto op24; case 25: goto op25; case 26: goto op26; case 27: goto op27; \
      case 28: goto op28; case 29: goto op29; case 30: goto op30; case 31: goto op31; case 32: goto op32; case 33: goto op33; \
      case 34: goto op34; case 35: goto op35; case 36: goto op36; case 37: goto op37; case 38: goto op38; case 39: goto op39; \
      case 40: goto op40; case 41: goto op41; case 42: goto op42; case 43: goto op43; case 44: goto op44; case 45: goto op45; \
      case 46: goto op46; case 47: goto op47; case 48: goto op48; case 49: goto op49; case 50: goto op50; case 51: goto op51; \
      case 52: goto op52; case 53: goto op53; case 54: goto op54; case 55: goto op55; case 56: goto op56; case 57: goto op57; \
      case 58: goto op58; case 59: goto op59; case 60: goto op60; case 61: goto op61; case 62: goto op62; case 63: goto op63; \
   }

/*** Globals and Prototypes ***/

extern char **environ;

/* memstart <= genstart <= memend */
static word *genstart;
static word *memstart;
static word *memend;
static word max_heap_mb; /* max heap size in MB */
static int breaked;      /* set in signal handler, passed over to owl in thread switch */
static word state;       /* IFALSE | previous program state across runs */

byte *hp;
static word *fp;
byte *file_heap;
word vm(word *ob, word *arg);
void exit(int rval);
void *realloc(void *ptr, size_t size);
void free(void *ptr);
char *getenv(const char *name);
int setenv(const char *name, const char *value, int overwrite);
int unsetenv(const char *);
DIR *opendir(const char *name);
DIR *fdopendir(int fd);
pid_t fork(void);
pid_t waitpid(pid_t pid, int *status, int options);
int chdir(const char *path);
int select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, struct timeval *timeout);
int execv(const char *path, char *const argv[]);
struct termios tsettings;

/*** Garbage Collector, based on "Efficient Garbage Compaction Algorithm" by Johannes Martin (1982) ***/

static __inline__ void rev(word pos) {
   word val = V(pos);
   word next = cont(val);
   V(pos) = next;
   cont(val) = (pos | FLAG) ^ (val & FLAG);
}

static __inline__ word *chase(word *pos) {
   word val = cont(pos);
   while (allocp(val) && flagged(val)) {
      pos = (word *)val;
      val = cont(pos);
   }
   return pos;
}

static void mark(word *pos, word *end) {
   while (pos != end) {
      word val = *pos;
      if (allocp(val) && val >= (word)genstart) {
         if (flagged(val)) {
            pos = (word *)flag(chase((word *)val)) - 1;
         } else {
            word hdr = V(val);
            rev((word) pos);
            if (flagged_or_raw(hdr)) {
               pos--;
            } else {
               pos = (word *)val + hdrsize(hdr) - 1;
            }
         }
      } else {
         pos--;
      }
   }
}

static word *compact() {
   word *new = genstart;
   word *old = new;
   word *end = memend - 1;
   while (((word)old) < ((word)end)) {
      word val = *old;
      if (flagged(val)) {
         word h;
         *new = val;
         do { /* unthread */
            rev((word) new);
         } while (flagged(*new));
         h = hdrsize(*new);
         if (old == new) {
            old += h;
            new += h;
         } else {
            while (--h) *++new = *++old;
            old++;
            new++;
         }
      } else {
         if (teardown_needed(val)) {
            printf("gc: would teardown\n");
         }
         old += hdrsize(val);
      }
   }
   return new;
}

void fix_pointers(word *pos, wdiff delta) {
   for (;;) {
      word hdr = *pos;
      int n = hdrsize(hdr);
      if (hdr == 0) return; /* end marker reached. only dragons beyond this point.*/
      if (rawp(hdr)) {
         pos += n; /* no pointers in raw objects */
      } else {
         pos++;
         n--;
         while(n--) {
            word val = *pos;
            if (allocp(val))
               *pos = val + delta;
            pos++;
         }
      }
   }
}

/* emulate sbrk with malloc'd memory, because sbrk is no longer properly supported */
/* n-cells-wanted → heap-delta (to be added to pointers), updates memstart and memend */
wdiff adjust_heap(wdiff cells) {
   word *old = memstart;
   word nwords = memend - memstart + MEMPAD; /* MEMPAD is after memend */
   word new_words = nwords + (cells > 0xffffff ? 0xffffff : cells); /* limit heap growth speed */
   if (((cells > 0) && (new_words*W < nwords*W)) || ((cells < 0) && (new_words*W > nwords*W)))
      return 0; /* don't try to adjust heap, if the size_t would overflow in realloc */
   memstart = realloc(memstart, new_words*W);
   if (memstart == old) { /* whee, no heap slide \o/ */
      memend = memstart + new_words - MEMPAD; /* leave MEMPAD words alone */
      return 0;
   } else if (memstart) { /* d'oh! we need to O(n) all the pointers... */
      wdiff delta = (word)memstart - (word)old;
      memend = memstart + new_words - MEMPAD; /* leave MEMPAD words alone */
      fix_pointers(memstart, delta);
      return delta;
   } else {
      breaked |= 8; /* will be passed over to mcp at thread switch*/
      return 0;
   }
}

/* input desired allocation size and (the only) pointer to root object
   return a pointer to the same object after heap compaction, possible heap size change and relocation */
static word *gc(int size, word *regs) {
   word *root;
   word *realend = memend;
   wdiff nfree;
   fp = regs + hdrsize(*regs);
   root = fp+1;
   *root = (word) regs;
   memend = fp;
   mark(root, fp);
   fp = compact();
   regs = (word *)*root;
   memend = realend;
   nfree = (word)memend - (word)regs;
   if (genstart == memstart) {
      word heapsize = (word) memend - (word) memstart;
      word nused = heapsize - nfree;
      if (heapsize / (1024 * 1024) > max_heap_mb)
         breaked |= 8; /* will be passed over to mcp at thread switch */
      nfree -= size*W + MEMPAD; /* how much really could be snipped off */
      if (nfree < (heapsize / 5) || nfree < 0) {
         /* increase heap size if less than 20% is free by ~10% of heap size (growth usually implies more growth) */
         regs[hdrsize(*regs)] = 0; /* use an invalid descriptor to denote end live heap data */
         regs = (word *) ((word)regs + adjust_heap(size*W + nused/10 + 4096));
         nfree = memend - regs;
         if (nfree <= size) {
            breaked |= 8; /* will be passed over to mcp at thread switch. may cause owl<->gc loop if handled poorly on lisp side! */
         }
      } else if (nfree > (heapsize/3)) {
         /* decrease heap size if more than 33% is free by 10% of the free space */
         wdiff dec = -(nfree / 10);
         wdiff new = nfree - dec;
         if (new > size*W*2 + MEMPAD) {
            regs[hdrsize(*regs)] = 0; /* as above */
            regs = (word *) ((word)regs + adjust_heap(dec+MEMPAD*W));
            heapsize = (word)memend - (word)memstart;
            nfree = (word)memend - (word)regs;
         }
      }
      genstart = regs; /* always start new generation */
   } else if (nfree < MINGEN || nfree < size*W*2) {
      genstart = memstart; /* start full generation */
      return gc(size, regs);
   } else {
      genstart = regs; /* start new generation */
   }
   return regs;
}

/*** OS Interaction and Helpers ***/

void signal_handler(int signal) {
   switch(signal) {
      case SIGINT:
         breaked |= 2; break;
      case SIGPIPE: break; /* can cause loop when reporting errors */
      default:
         breaked |= 4;
   }
}

/* small functions defined locally after hitting some portability issues */
static void bytecopy(byte *from, byte *to, int n) { while(n--) *to++ = *from++; }
static void wordcopy(word *from, word *to, int n) { while(n--) *to++ = *from++; }

unsigned int lenn(byte *pos, unsigned int max) { /* added here, strnlen was missing in win32 compile */
   unsigned int p = 0;
   while(p < max && *pos++) p++;
   return p;
}

/* list length, no overflow or valid termination checks */
int llen(word *ptr) {
   int len = 0;
   while(pairp(ptr)) {
      len++;
      ptr = (word *) ptr[2];
   }
   return len;
}

static word payl_len(word hdr) {
   word len = (hdrsize(hdr) - 1) * W;
   if (rawp(hdr))
      len -= (hdr >> 8) & 7;
   return len;
}

void set_signal_handler() {
   struct sigaction sa;
   sa.sa_handler = signal_handler;
   sigemptyset(&sa.sa_mask);
   sa.sa_flags = SA_RESTART;
   sigaction(SIGINT, &sa, NULL);
   sigaction(SIGPIPE, &sa, NULL);
}

static word mkpair(word h, word a, word d) {
   word *pair;
   allocate(3, pair);
   pair[0] = h;
   pair[1] = a;
   pair[2] = d;
   return (word)pair;
}

/* make a byte vector object to hold len bytes (compute size, advance fp, set padding count) */
static word *mkbvec(size_t len, int type) {
   int nwords = OBJWORDS(len);
   int pads = (nwords-1)*W - len;
   word *ob;
   byte *end;
   allocate(nwords, ob);
   end = (byte *)ob + W + len;
   *ob = make_raw_header(nwords, type, pads);
   while (pads--)
      *end++ = 0; /* clear the padding bytes */
   return ob;
}

/*** Primops called from VM and generated C-code ***/

static word prim_connect(word *host, word port, word type) {
   int sock;
   byte *ip = (unsigned char *)host + W;
   unsigned long ipfull;
   struct sockaddr_in addr;
   char udp = (immval(type) == 1);
   port = immval(port);
   if ((sock = socket(PF_INET, (udp ? SOCK_DGRAM : SOCK_STREAM), (udp ? IPPROTO_UDP : 0))) == -1)
      return IFALSE;
   if (udp)
      return make_immediate(sock, TPORT);
   if (!allocp(host)) /* bad host type */
      return IFALSE;
   addr.sin_family = AF_INET;
   addr.sin_port = htons(port);
   addr.sin_addr.s_addr = (in_addr_t) host[1];
   ipfull = (ip[0]<<24) | (ip[1]<<16) | (ip[2]<<8) | ip[3];
   addr.sin_addr.s_addr = htonl(ipfull);
   if (connect(sock, (struct sockaddr *) &addr, sizeof(struct sockaddr_in)) < 0) {
      close(sock);
      return IFALSE;
   }
   return make_immediate(sock, TPORT);
}

static word prim_less(word a, word b) {
   if (immediatep(a)) {
      return immediatep(b) ? BOOL(a < b) : ITRUE;  /* imm < alloc */
   } else {
      return immediatep(b) ? IFALSE : BOOL(a < b); /* alloc > imm */
   }
}

static word prim_get(word *ff, word key, word def) { /* ff assumed to be valid */
   while((word) ff != IEMPTY) { /* ff = [header key value [maybe left] [maybe right]] */
      word this = ff[1], hdr;
      if (this == key)
         return ff[2];
      hdr = *ff;
      switch(hdrsize(hdr)) {
         case 3:
            return def;
         case 4:
            if (key < this) {
               ff = (word *)(hdr & (1 << TPOS) ? IEMPTY : ff[3]);
            } else {
               ff = (word *)(hdr & (1 << TPOS) ? ff[3] : IEMPTY);
            }
            break;
         default:
            ff = (word *)(key < this ? ff[3] : ff[4]);
      }
   }
   return def;
}

static word prim_cast(word *ob, int type) {
   if (immediatep((word)ob)) {
      return make_immediate(immval((word)ob), type & 63);
   } else { /* make a clone of more desired type */
      word hdr = *ob++;
      int size = hdrsize(hdr);
      word *new, *res; /* <- could also write directly using *fp++ */
      allocate(size, new);
      res = new;
      *new++ = (hdr&(~252))|((type&1087)<<TPOS); /* clear type, allow setting teardown in new one */
      wordcopy(ob, new, size - 1);
      return (word)res;
   }
}

static word prim_refb(word pword, unsigned int pos) {
   if (immediatep(pword) || pos >= payl_len(header(pword)))
      return IFALSE;
   return F(((byte *)pword)[W + pos]);
}

static word prim_ref(word pword, word pos) {
   word *ob = (word *)pword;
   word hdr, size;
   pos = immval(pos);
   if (immediatep(ob))
      return IFALSE;
   hdr = *ob;
   if (rawp(hdr)) { /* raw data is #[hdrbyte{W} b0 .. bn 0{0,W-1}] */
      size = payl_len(hdr);
      if (pos >= size)
         return IFALSE;
      return F(((byte *) ob)[pos+W]);
   }
   size = hdrsize(hdr);
   if (!pos || size <= pos) /* tuples are indexed from 1 (probably later 0-255)*/
      return IFALSE;
   return ob[pos];
}

static int64_t cnum(word a) {
   uint64_t x;
   if (allocp(a)) {
      word *p = (word *)a;
      unsigned int shift = 0;
      x = 0;
      do {
         x |= immval(p[1]) << shift;
         shift += FBITS;
         p = (word *)p[2];
      } while (shift < 64 && allocp(p));
      return header(a) == NUMNHDR ? -x : x;
   }
   x = immval(a);
   return is_type(a, TNUMN) ? -x : x;
}

static word onum(int64_t a, int s) {
   uint64_t x = a;
   word h = NUMHDR, t = TNUM;
   if (s && a < 0) {
      h = NUMNHDR;
      t = TNUMN;
      x = -a;
   }
   if (x > FMAX) {
      word p = INULL;
      unsigned int shift = (63 / FBITS) * FBITS;
      while (!(x & ((uint64_t)FMAX << shift)))
         shift -= FBITS;
      do {
         p = mkpair(NUMHDR, F((x >> shift) & FMAX), p);
         shift -= FBITS;
      } while (shift + FBITS);
      header(p) = h;
      return p;
   }
   return make_immediate(x, t);
}

static word prim_set(word wptr, word pos, word val) {
   word *ob = (word *)wptr;
   word hdr, p;
   word *new;
   pos = immval(pos);
   if (immediatep(ob))
      return IFALSE;
   hdr = *ob;
   if (rawp(hdr) || hdrsize(hdr) < pos)
      return IFALSE;
   hdr = hdrsize(hdr);
   allocate(hdr, new);
   for (p = 0; p <= hdr; ++p)
      new[p] = (pos == p && p) ? val : ob[p];
   return (word) new;
}

void setdown() {
   tcsetattr(0, TCSANOW, &tsettings); /* return stdio settings */
}

/* system- and io primops */
static word prim_sys(int op, word a, word b, word c) {
   switch(op) {
      case 0: /* write fd data len | #f → nbytes | #f */
         if (is_type(a, TPORT) && allocp(b)) {
            size_t len, size = payl_len(header(b));
            len = c != IFALSE ? cnum(c) : size;
            if (len <= size) {
               len = write(immval(a), (const word *)b + 1, len);
               if (len != (size_t)-1)
                  return onum(len, 0);
            }
         }
         return IFALSE;
      case 1: /* open path flags mode → port | #f */
         if (stringp(a)) {
            int fd = open((const char *)a + W, cnum(b), immval(c));
            if (fd != -1)
               return make_immediate(fd, TPORT);
         }
         return IFALSE;
      case 2:
         return BOOL(close(immval(a)) == 0);
      case 3: { /* 3 = sopen port 0=tcp|1=udp -> False | fd  */
         int port = immval(a);
         int type = immval(b);
         int s;
         int opt = 1; /* TRUE */
         char udp = (type == 1);
         struct sockaddr_in myaddr;
         myaddr.sin_family = AF_INET;
         myaddr.sin_port = htons(port);
         myaddr.sin_addr.s_addr = INADDR_ANY;
         s = socket(AF_INET, (udp ? SOCK_DGRAM : SOCK_STREAM), (udp ? IPPROTO_UDP : 0));
         if (s < 0)
            return IFALSE;
         if (type != 1) {
            if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&opt, sizeof(opt)) \
                || bind(s, (struct sockaddr *) &myaddr, sizeof(myaddr)) != 0 \
                || listen(s, SOMAXCONN) != 0) {
               close(s);
               return IFALSE;
            }
         } else {
            if (bind(s, (struct sockaddr *) &myaddr, sizeof(myaddr)) != 0) {
               close(s);
               return IFALSE;
            }
         }
         return make_immediate(s, TPORT); }
      case 4: { /* 4 = accept port -> rval=False|(ip . fd) */
         int sock = immval(a);
         struct sockaddr_in addr;
         socklen_t len = sizeof(addr);
         int fd;
         word *ipa;
         fd = accept(sock, (struct sockaddr *)&addr, &len);
         if (fd < 0) return IFALSE;
         ipa = mkbvec(4, TBVEC);
         bytecopy((byte *)&addr.sin_addr, (byte *)ipa + W, 4);
         return cons((word)ipa, make_immediate(fd, TPORT)); }
      case 5: /* read fd len -> bvec | EOF | #f */
         if (is_type(a, TPORT)) {
            size_t len = memend - fp;
            const size_t max = len > MAXOBJ ? MAXPAYL : (len - 1) * W;
            len = cnum(b);
            len = read(immval(a), fp + 1, len < max ? len : max);
            if (len == 0)
               return IEOF;
            if (len != (size_t)-1)
               return (word)mkbvec(len, TBVEC);
         }
         return IFALSE;
      case 6:
         setdown();
         exit(immval(a)); /* stop the press */
      case 7: /* set memory limit (in mb) */
         max_heap_mb = immval(a);
         return a;
      case 8: { /* return system constants */
         static const word sysconst[] = {
            S_IFMT, W, S_IFBLK, S_IFCHR, S_IFIFO, S_IFREG, S_IFDIR, S_IFLNK,
            S_IFSOCK, E2BIG, EACCES, EADDRINUSE, EADDRNOTAVAIL, EAFNOSUPPORT, EAGAIN, EALREADY,
            EBADF, EBADMSG, EBUSY, ECANCELED, ECHILD, ECONNABORTED, ECONNREFUSED, ECONNRESET,
            EDEADLK, EDESTADDRREQ, EDOM, EDQUOT, EEXIST, EFAULT, EFBIG, EHOSTUNREACH,
            EIDRM, EILSEQ, EINPROGRESS, EINTR, EINVAL, EIO, EISCONN, EISDIR,
            ELOOP, EMFILE, EMLINK, EMSGSIZE, EMULTIHOP, ENAMETOOLONG, ENETDOWN, ENETRESET,
            ENETUNREACH, ENFILE, ENOBUFS, ENODATA, ENODEV, ENOENT, ENOEXEC, ENOLCK,
            ENOLINK, ENOMEM, ENOMSG, ENOPROTOOPT, ENOSPC, ENOSR, ENOSTR, ENOSYS,
            ENOTCONN, ENOTDIR, ENOTEMPTY, ENOTRECOVERABLE, ENOTSOCK, ENOTSUP, ENOTTY, ENXIO,
            EOPNOTSUPP, EOVERFLOW, EOWNERDEAD, EPERM, EPIPE, EPROTO, EPROTONOSUPPORT, EPROTOTYPE,
            ERANGE, EROFS, ESPIPE, ESRCH, ESTALE, ETIME, ETIMEDOUT, ETXTBSY,
            EWOULDBLOCK, EXDEV, SEEK_SET, SEEK_CUR, SEEK_END, O_EXEC, O_RDONLY, O_RDWR,
            O_SEARCH, O_WRONLY, O_APPEND, O_CLOEXEC, O_CREAT, O_DIRECTORY, O_DSYNC, O_EXCL,
            O_NOCTTY, O_NOFOLLOW, O_NONBLOCK, O_RSYNC, O_SYNC, O_TRUNC, O_TTY_INIT, O_ACCMODE,
            FD_CLOEXEC, F_DUPFD, F_DUPFD_CLOEXEC, F_GETFD, F_SETFD, F_GETFL, F_SETFL, F_GETOWN,
            F_SETOWN, F_GETLK, F_SETLK, F_SETLKW, F_RDLCK, F_UNLCK, F_WRLCK
         };
         return onum(sysconst[immval(a) % (sizeof sysconst / W)], 0); }
      case 9: /* return process variables */
         return onum(
            a == F(0) ? errno :
            a == F(1) ? (word)environ :
            max_heap_mb, 0);
      case 10: { /* receive-udp-packet sock → (ip-bvec . payload-bvec)| #false */
         struct sockaddr_in si_other;
         socklen_t slen = sizeof(si_other);
         word *bvec;
         word *ipa;
         int recvd;
         recvd = recvfrom(immval(a), fp + 1, 65528, 0, (struct sockaddr *)&si_other, &slen);
         if (recvd < 0)
            return IFALSE;
         bvec = mkbvec(recvd, TBVEC);
         ipa = mkbvec(4, TBVEC);
         bytecopy((byte *)&si_other.sin_addr, (byte *)ipa + W, 4);
         return cons((word)ipa, (word)bvec); }
      case 11: /* open-dir path → dirobjptr | #false */
         if (stringp(a)) {
            DIR *dirp = opendir((const char *)a + W);
            if (dirp != NULL)
               return onum((intptr_t)dirp, 1);
         }
         return IFALSE;
      case 12: { /* read-dir dirp → pointer */
         struct dirent *ent;
         errno = 0;
         ent = readdir((DIR *)(intptr_t)cnum(a));
         return onum(ent != NULL ? (word)&ent->d_name : 0, 0); }
      case 13: /* close-dir dirp → bool */
         return BOOL(closedir((DIR *)(intptr_t)cnum(a)) == 0);
      case 14: /* strerror errnum → pointer */
         return onum((word)strerror(immval(a)), 0);
      case 15: /* fcntl port cmd arg → integer | #f */
         if (is_type(a, TPORT)) {
            int res = fcntl(immval(a), cnum(b), (intptr_t)cnum(c));
            if (res != -1)
               return onum(res, 1);
         }
         return IFALSE;
      case 16: /* getenv key → pointer */
         return onum(stringp(a) ? (word)getenv((const char *)a + W) : 0, 0);
      case 17: { /* exec[v] path argl ret */
         char *path = (char *)a + W;
         int nargs = llen((word *)b);
         char **args = realloc(NULL, (nargs + 1) * sizeof(char *));
         char **argp = args;
         if (args == NULL)
            return IFALSE;
         while (nargs--) {
            *argp++ = (char *)G(b, 1) + W;
            b = G(b, 2);
         }
         *argp = NULL;
         execv(path, args); /* may return -1 and set errno */
         free(args);
         return IFALSE; }
      case 18: { /* fork → #f: failed, 0: we're in child process, integer: we're in parent process */
         pid_t pid = fork();
         return pid != -1 ? onum(pid, 1) : IFALSE; }
      case 19: { /* wait <pid> <respair> _ */
         pid_t pid = a != IFALSE ? cnum(a) : -1;
         int status;
         word *r = (word *)b;
         pid = waitpid(pid, &status, WNOHANG|WUNTRACED); /* |WCONTINUED */
         if (pid == -1)
            return IFALSE; /* error */
         if (pid == 0)
            return ITRUE; /* no changes, would block */
         if (WIFEXITED(status)) {
            r[1] = F(1);
            r[2] = F(WEXITSTATUS(status));
         } else if (WIFSIGNALED(status)) {
            r[1] = F(2);
            r[2] = F(WTERMSIG(status));
         } else if (WIFSTOPPED(status)) {
            r[1] = F(3);
            r[2] = F(WSTOPSIG(status));
         /*} else if (WIFCONTINUED(status)) {
            r[1] = F(4);
            r[2] = F(1); */
         } else {
            r = (word *)IFALSE;
         }
         return (word)r; }
      case 20: /* chdir path → bool */
         return BOOL(stringp(a) && chdir((char *)a + W) == 0);
      case 21: /* kill pid signal → bool */
         return BOOL(kill(cnum(a), immval(b)) == 0);
      case 22: /* unlink path → bool */
         return BOOL(stringp(a) && unlink((char *)a + W) == 0);
      case 23: /* rmdir path → bool */
         return BOOL(stringp(a) && rmdir((char *)a + W) == 0);
      case 24: /* mknod path (type . mode) dev → bool */
         if (stringp(a) && pairp(b)) {
            const char *path = (const char *)a + W;
            const mode_t type = cnum(G(b, 1)), mode = immval(G(b, 2));
            if ((type == S_IFDIR ? mkdir(path, mode) : mknod(path, type | mode, cnum(c))) == 0)
               return ITRUE;
         }
         return IFALSE;
      case 25: /* lseek port offset whence → offset | #f */
         if (is_type(a, TPORT)) {
            off_t o = lseek(immval(a), cnum(b), cnum(c));
            if (o != -1)
               return onum(o, 1);
         }
         return IFALSE;
      case 26:
         if (a != IFALSE) {
            static struct termios old;
            tcgetattr(0, &old);
            old.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
            old.c_oflag &= ~OPOST;
            old.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
            old.c_cflag &= ~(CSIZE | PARENB);
            old.c_cflag |= CS8;
            return BOOL(tcsetattr(0, TCSANOW, &old) == 0);
         }
         return BOOL(tcsetattr(0, TCSANOW, &tsettings) == 0);
      case 27: { /* sendmsg sock (port . ipv4) bvec */
         int sock = immval(a);
         int port;
         struct sockaddr_in peer;
         byte *ip, *data = (byte *)c + W;
         size_t len = payl_len(header(c));
         port = immval(G(b, 1));
         ip = (byte *)G(b, 2) + W;
         peer.sin_family = AF_INET;
         peer.sin_port = htons(port);
         peer.sin_addr.s_addr = htonl((ip[0]<<24) | (ip[1]<<16) | (ip[2]<<8) | (ip[3]));
         return BOOL(sendto(sock, data, len, 0, (struct sockaddr *)&peer, sizeof(peer)) != -1); }
      case 28: /* setenv <owl-raw-bvec-or-ascii-leaf-string> <owl-raw-bvec-or-ascii-leaf-string-or-#f> */
         if (stringp(a) && (b == IFALSE || stringp(b))) {
            const char *name = (const char *)a + W;
            if ((b != IFALSE ? setenv(name, (const char *)b + W, 1) : unsetenv(name)) == 0)
               return ITRUE;
         }
         return IFALSE;
      case 29:
         return prim_connect((word *) a, b, c);
      case 30: /* dup2 old-port new-fd → new-port | #f */
         if (is_type(a, TPORT)) {
            int fd = dup2(immval(a), immval(b));
            if (fd != -1)
               return make_immediate(fd, TPORT);
         }
         return IFALSE;
      case 31: { /* pipe → '(read-port . write-port) | #f */
         int fd[2];
         if (pipe(fd) != 0)
            return IFALSE;
         return cons(make_immediate(fd[0], TPORT), make_immediate(fd[1], TPORT)); }
      case 32: /* rename src dst → bool */
         return BOOL(stringp(a) && stringp(b) && rename((char *)a + W, (char *)b + W) == 0);
      case 33: /* link src dst → bool */
         return BOOL(stringp(a) && stringp(b) && link((char *)a + W, (char *)b + W) == 0);
      case 34: /* symlink src dst → bool */
         return BOOL(stringp(a) && stringp(b) && symlink((char *)a + W, (char *)b + W) == 0);
      case 35: /* readlink path → raw-sting | #false */
         if (stringp(a)) {
            size_t len = memend - fp;
            size_t max = len > MAXOBJ ? MAXPAYL + 1 : (len - 1) * W;
            /* the last byte is temporarily used to check, if the string fits */
            len = readlink((const char *)a + W, (char *)fp + W, max);
            if (len != (size_t)-1 && len != max)
               return (word)mkbvec(len, TSTRING);
         }
         return IFALSE;
      case 36: /* getcwd → raw-sting | #false */
         {
            size_t len = memend - fp;
            size_t max = len > MAXOBJ ? MAXPAYL + 1 : (len - 1) * W;
            /* the last byte is temporarily used for the terminating '\0' */
            if (getcwd((char *)fp + W, max) != NULL)
               return (word)mkbvec(lenn((byte *)fp + W, max - 1), TSTRING);
         }
         return IFALSE;
      case 37: /* umask mask → mask */
         return F(umask(immval(a)));
      case 38: /* stat fd|path follow → list */
         if (immediatep(a) || stringp(a)) {
            struct stat st;
            int flg = b != IFALSE ? 0 : AT_SYMLINK_NOFOLLOW;
            if ((allocp(a) ? fstatat(AT_FDCWD, (char *)a + W, &st, flg) : fstat(immval(a), &st)) == 0) {
               word lst = INULL;
               lst = cons(onum(st.st_blocks, 1), lst);
               lst = cons(onum(st.st_blksize, 1), lst);
               lst = cons(onum(st.st_ctim.tv_sec * INT64_C(1000000000) + st.st_atim.tv_nsec, 1), lst);
               lst = cons(onum(st.st_mtim.tv_sec * INT64_C(1000000000) + st.st_atim.tv_nsec, 1), lst);
               lst = cons(onum(st.st_atim.tv_sec * INT64_C(1000000000) + st.st_atim.tv_nsec, 1), lst);
               lst = cons(onum(st.st_size, 1), lst);
               lst = cons(onum(st.st_rdev, 0), lst);
               lst = cons(onum(st.st_gid, 0), lst);
               lst = cons(onum(st.st_uid, 0), lst);
               lst = cons(onum(st.st_nlink, 0), lst);
               lst = cons(onum(st.st_mode, 0), lst);
               lst = cons(onum(st.st_ino, 0), lst);
               lst = cons(onum(st.st_dev, 1), lst);
               return lst;
            }
         }
         return INULL;
      case 39: /* chmod fd|path mode follow → bool */
         if ((immediatep(a) || stringp(a)) && fixnump(b)) {
            mode_t mod = immval(b);
            int flg = c != IFALSE ? 0 : AT_SYMLINK_NOFOLLOW;
            if ((allocp(a) ? fchmodat(AT_FDCWD, (char *)a + W, mod, flg) : fchmod(immval(a), mod)) == 0)
               return ITRUE;
         }
         return IFALSE;
      case 40: /* chown fd|path (uid . gid) follow → bool */
         if ((immediatep(a) || stringp(a)) && pairp(b)) {
            uid_t uid = cnum(G(b, 1));
            gid_t gid = cnum(G(b, 2));
            int flg = c != IFALSE ? 0 : AT_SYMLINK_NOFOLLOW;
            if ((allocp(a) ? fchownat(AT_FDCWD, (char *)a + W, uid, gid, flg) : fchown(immval(a), uid, gid)) == 0)
               return ITRUE;
         }
         return IFALSE;
      case 41: { /* peek mem nbytes → num */
         const word p = cnum(a);
         return onum(
            b == F(1) ? *(uint8_t *)p :
            b == F(2) ? *(uint16_t *)p :
            b == F(4) ? *(uint32_t *)p :
            b == F(8) ? *(uint64_t *)p :
            V(p), 0);
         }
      default:
         return IFALSE;
   }
}

static word prim_lraw(word wptr, int type) {
   word *lst = (word *)wptr;
   byte *pos;
   word *raw, *ob;
   unsigned int len = 0;
   for (ob = lst; pairp(ob); ob = (word *)ob[2])
      len++;
   if ((word)ob != INULL || len > MAXPAYL)
      return IFALSE;
   raw = mkbvec(len, type);
   pos = (byte *)raw + W;
   for (ob = lst; (word)ob != INULL; ob = (word *)ob[2])
      *pos++ = immval(ob[1]) & 255;
   return (word)raw;
}

static word prim_mkff(word t, word l, word k, word v, word r) {
   word *ob = fp;
   ob[1] = k;
   ob[2] = v;
   if (l == IEMPTY) {
      if (r == IEMPTY) {
         *ob = make_header(3, t);
         fp += 3;
      } else {
         *ob = make_header(4, t | FFRIGHT);
         ob[3] = r;
         fp += 4;
      }
   } else if (r == IEMPTY) {
      *ob = make_header(4, t);
      ob[3] = l;
      fp += 4;
   } else {
      *ob = make_header(5, t);
      ob[3] = l;
      ob[4] = r;
      fp += 5;
   }
   return (word) ob;
}

void do_poll(word a, word b, word c, word *r1, word *r2) {
   fd_set rs, ws, es;
   word *cur;
   int nfds = 1;
   struct timeval tv;
   int res;
   FD_ZERO(&rs); FD_ZERO(&ws); FD_ZERO(&es);
   cur = (word *)a;
   while((word)cur != INULL) {
      int fd = immval(G(cur[1], 1));
      FD_SET(fd, &rs);
      FD_SET(fd, &es);
      if (fd >= nfds)
         nfds = fd + 1;
      cur = (word *) cur[2];
   }
   cur = (word *)b;
   while ((word)cur != INULL) {
      int fd = immval(G(cur[1], 1));
      FD_SET(fd, &ws);
      FD_SET(fd, &es);
      if (fd >= nfds)
         nfds = fd + 1;
      cur = (word *) cur[2];
   }
   if (c == IFALSE) {
      res = select(nfds, &rs, &ws, &es, NULL);
   } else {
      int ms = immval(c);
      tv.tv_sec = ms/1000;
      tv.tv_usec = (ms%1000)*1000;
      res = select(nfds, &rs, &ws, &es, &tv);
   }
   if (res < 1) {
      *r1 = IFALSE; *r2 = BOOL(res < 0); /* 0 = timeout, otherwise error or signal */
   } else {
      int fd; /* something active, wake the first thing */
      for(fd=0;;fd++) {
         if (FD_ISSET(fd, &rs)) {
            *r1 = make_immediate(fd, TPORT); *r2 = F(1); break;
         } else if (FD_ISSET(fd, &ws)) {
            *r1 = make_immediate(fd, TPORT); *r2 = F(2); break;
         } else if (FD_ISSET(fd, &es)) {
            *r1 = make_immediate(fd, TPORT); *r2 = F(3); break;
         }
      }
   }
}

word vm(word *ob, word *arg) {
   unsigned char *ip;
   unsigned int bank = 0;
   unsigned int ticker = TICKS;
   unsigned short acc = 0;
   int op;
   word R[NR];

   word load_imms[] = {F(0), INULL, ITRUE, IFALSE}; /* for ldi and jv */

   /* clear blank regs */
   while(acc < NR) { R[acc++] = INULL; }
   R[0] = IFALSE;
   R[3] = IHALT;
   R[4] = (word) arg;
   acc = 2; /* boot always calls with 2 args*/

apply: /* apply something at ob to values in regs, or maybe switch context */

   if (allocp(ob)) {
      word hdr = *ob & 4095; /* cut size out, take just header info */
      if (hdr == make_header(0, TPROC)) { /* proc */
         R[1] = (word) ob; ob = (word *) ob[1];
      } else if (hdr == make_header(0, TCLOS)) { /* clos */
         R[1] = (word) ob; ob = (word *) ob[1];
         R[2] = (word) ob; ob = (word *) ob[1];
      } else if (((hdr>>TPOS)&60) == TFF) { /* low bits have special meaning */
         word *cont = (word *) R[3];
         if (acc == 3) {
            R[3] = prim_get(ob, R[4], R[5]);
         } else if (acc == 2) {
            R[3] = prim_get(ob, R[4], (word) 0);
            if (!R[3]) { error(260, ob, R[4]); }
         } else {
            error(259, ob, INULL);
         }
         ob = cont;
         acc = 1;
         goto apply;
      } else if (!is_type(hdr, TBYTECODE)) { /* not even code, extend bits later */
         error(259, ob, INULL);
      }
      if (!ticker--) goto switch_thread;
      ip = (unsigned char *)ob + W;
      goto invoke;
   } else if ((word)ob == IEMPTY && acc > 1) { /* ff application: (False key def) -> def */
      ob = (word *) R[3]; /* call cont */
      R[3] = acc > 2 ? R[5] : IFALSE; /* default arg or false if none */
      acc = 1;
      goto apply;
   } else if ((word)ob == IHALT) {
      /* it's the final continuation */
      ob = (word *) R[0];
      if (allocp(ob)) {
         R[0] = IFALSE;
         breaked = 0;
         R[4] = R[3];
         R[3] = F(2);
         R[5] = IFALSE;
         R[6] = IFALSE;
         ticker = 0xffffff;
         bank = 0;
         acc = 4;
         goto apply;
      }
      if (acc == 2) { /* update state when main program exits with 2 values */
         state = R[4];
      }
      return R[3];
   } else {
      word *state, pos = 1;
      allocate(acc+1, state);
      *state = make_header(acc+1, TTUPLE);
      while(pos <= acc) {
         state[pos] = R[pos+2]; /* first arg at R3*/
         pos++;
      }
      error(0, ob, state); /* not callable */
   }

switch_thread: /* enter mcp if present */
   if (R[0] == IFALSE) { /* no mcp, ignore */
      ticker = TICKS;
      goto apply;
   } else {
      /* save vm state and enter mcp cont at R0 */
      word *state, pos = 1;
      ticker=0xffffff;
      bank = 0;
      acc = acc + 4;
      R[acc] = (word) ob;
      allocate(acc, state);
      *state = make_header(acc, TTHREAD);
      state[acc-1] = R[acc];
      while(pos < acc-1) {
         state[pos] = R[pos];
         pos++;
      }
      ob = (word *) R[0];
      R[0] = IFALSE; /* remove mcp cont */
      /* R3 marks the syscall to perform */
      R[3] = breaked ? (breaked & 8 ? F(14) : F(10)) : F(1);
      R[4] = (word) state;
      R[5] = F(breaked);
      R[6] = IFALSE;
      acc = 4;
      breaked = 0;
      goto apply;
   }
invoke: /* nargs and regs ready, maybe gc and execute ob */
   if (((word)fp) + 1024*64 >= ((word) memend)) {
      int p = 0;
      *fp = make_header(NR + 2, 50); /* hdr r_0 .. r_(NR-1) ob */
      while (p < NR) {
         fp[p + 1] = R[p];
         p++;
      }
      fp[p+1] = (word) ob;
      fp = gc(1024*64, fp);
      ob = (word *) fp[p+1];
      while (--p >= 0)
         R[p] = fp[p + 1];
      ip = (unsigned char *)ob + W;
   }

   op = *ip++;

   if (op) {
      main_dispatch:
      EXEC;
   } else {
      op = *ip<<8 | ip[1];
      goto super_dispatch;
   }

   op0: op = (*ip << 8) | ip[1]; goto super_dispatch;
   op1: A2 = G(A0, ip[1]); NEXT(3);
   op2: ob = (word *)A0; acc = ip[1]; goto apply;
   op3: OCLOSE(TCLOS); NEXT(0);
   op4: OCLOSE(TPROC); NEXT(0);
   op5: /* mov2 from1 to1 from2 to2 */
      A1 = A0;
      A3 = A2;
      NEXT(4);
   op6: CLOSE1(TCLOS); NEXT(0);
   op7: CLOSE1(TPROC); NEXT(0);
   op8: /* jlq a b o, extended jump */
      if (A0 == A1)
         ip += ip[2] + (ip[3] << 8);
      NEXT(4);
   op9: A1 = A0; NEXT(2);
   op10: error(10, F(42), F(42));
   op11:
      do_poll(A0, A1, A2, &A3, &A4);
      NEXT(5);
   op12: /* jb n */
      ip -= ip[0];
      if (ticker) /* consume thread time */
         ticker--;
      NEXT(0);
   op13: /* ldi{2bit what} [to] */
      A0 = load_imms[op >> 6];
      NEXT(1);
   op14: A1 = F(*ip); NEXT(2);
   op15: { /* type-byte o r <- actually sixtet */
      word ob = A0;
      if (allocp(ob))
         ob = V(ob);
      A1 = F(imm_type(ob));
      NEXT(2); }
   op16: /* jv[which] a o1 a2*/
      /* FIXME, convert this to jump-const <n> comparing to make_immediate(<n>,TCONST) */
      if (A0 == load_imms[op >> 6])
         ip += ip[1] + (ip[2] << 8);
      NEXT(3);
   op17: { /* arity error */
      word *t;
      int p;
      allocate(acc+1, t);
      *t = make_header(acc+1, TTUPLE);
      for(p = 0; p < acc; p++) {
         t[p+1] = R[p+3];
      }
      error(17, ob, t); }
   op18: /* goto-code p */
      ob = (word *)A0; /* needed in opof gc */
      acc = ip[1];
      ip = (unsigned char *)A0 + W;
      goto invoke;
   op19: { /* goto-proc p */
      word *this = (word *)A0;
      R[1] = (word) this;
      acc = ip[1];
      ob = (word *) this[1];
      ip = (unsigned char *)ob + W;
      goto invoke; }
   op20: { /* apply */
      int reg, arity;
      word *lst;
      if (op == 20) { /* normal apply: cont=r3, fn=r4, a0=r5 */
         reg = 4; /* include cont */
         arity = 1;
         ob = (word *) R[reg];
         acc -= 3; /* ignore cont, function and stop before last one (the list) */
         while(acc--) { /* move explicitly given arguments down by one to correct positions */
            R[reg] = R[reg+1]; /* copy args down*/
            reg++;
            arity++;
         }
         lst = (word *) R[reg+1];
      } else { /* _sans_cps apply: func=r3, a0=r4 */
         reg = 3; /* include cont */
         arity = 0;
         ob = (word *) R[reg];
         acc -= 2; /* ignore function and stop before last one (the list) */
         while(acc--) { /* move explicitly given arguments down by one to correct positions */
            R[reg] = R[reg+1]; /* copy args down*/
            reg++;
            arity++;
         }
         lst = (word *) R[reg+1];
      }
      while(pairp(lst)) { /* unwind argument list */
         /* FIXME: unwind only up to last register and add limited rewinding to arity check */
         if (reg > 128) { /* dummy handling for now */
            exit(3);
         }
         R[reg++] = lst[1];
         lst = (word *) lst[2];
         arity++;
      }
      acc = arity;
      goto apply; }
   op21: { /* goto-clos p */
      word *this = (word *)A0;
      R[1] = (word) this;
      acc = ip[1];
      this = (word *) this[1];
      R[2] = (word) this;
      ob = (word *) this[1];
      ip = (unsigned char *)ob + W;
      goto invoke; }
   op22: /* cast o t r */
      A2 = prim_cast((word *)A0, immval(A1));
      NEXT(3);
   op23: { /* mkt t s f1 .. fs r */
      word t = *ip++;
      word s = *ip++ + 1; /* the argument is n-1 to allow making a 256-tuple with 255, and avoid 0-tuples */
      word *ob, p = 0;
      allocate(s+1, ob); /* s fields + header */
      *ob = make_header(s+1, t);
      while (p < s) {
         ob[p+1] = R[ip[p]];
         p++;
      }
      R[ip[p]] = (word) ob;
      NEXT(s+1); }
   op24: /* ret val == implicit call r3 with 1 arg */
      ob = (word *) R[3];
      R[3] = A0;
      acc = 1;
      goto apply;
   op25: { /* jmp-nargs(>=?) a hi lo */
      int needed = *ip;
      if (acc == needed) {
         if (op & 64) /* add empty extra arg list */
            R[acc + 3] = INULL;
      } else if ((op & 64) && acc > needed) {
         word tail = INULL; /* todo: no call overflow handling yet */
         while (acc > needed) {
            tail = cons(R[acc + 2], tail);
            acc--;
         }
         R[acc + 3] = tail;
      } else {
         ip += (ip[1] << 8) | ip[2];
      }
      NEXT(3); }
   op26: { /* fxqr ah al b qh ql r, b != 0, int32 / int16 -> int32, as fixnums */
      uint64_t a = ((uint64_t)immval(A0) << FBITS) | immval(A1);
      word b = immval(A2);
      uint64_t q;
      q = a / b;
      A3 = F(q>>FBITS);
      A4 = F(q&FMAX);
      A5 = F(a - q*b);
      NEXT(6); }
   op27: /* syscall cont op arg1 arg2 */
      ob = (word *) R[0];
      R[0] = IFALSE;
      R[3] = A1;
      R[4] = A0;
      R[5] = A2;
      R[6] = A3;
      acc = 4;
      if (ticker > 10) bank = ticker; /* deposit remaining ticks for return to thread */
      goto apply;
   op28: { /* sizeb obj to */
      word ob = A0;
      if (immediatep(ob)) {
         A1 = IFALSE;
      } else {
         word hdr = header(ob);
         A1 = rawp(hdr) ? F(payl_len(hdr)) : IFALSE;
      }
      NEXT(2); }
   op29: { /* ncons a b r */
      A2 = mkpair(NUMHDR, A0, A1);
      NEXT(3); }
   op30: { /* ncar a rd */
      word *ob = (word *)A0;
      assert(allocp(ob), ob, 30);
      A1 = ob[1];
      NEXT(2); }
   op31: { /* ncdr a r */
      word *ob = (word *)A0;
      assert(allocp(ob), ob, 31);
      A1 = ob[2];
      NEXT(2); }
   op32: { /* bind tuple <n> <r0> .. <rn> */
      word *tuple = (word *) R[*ip++];
      word hdr, pos = 1, n = *ip++;
      assert(allocp(tuple), tuple, 32);
      hdr = *tuple;
      assert_not((rawp(hdr) || hdrsize(hdr)-1 != n), tuple, 32);
      while(n--) { R[*ip++] = tuple[pos++]; }
      NEXT(0); }
   op33:
      error(33, IFALSE, IFALSE);
   op34: /* jmp-nargs a hi li */
      if (acc != *ip) {
         ip += (ip[1] << 8) | ip[2];
      }
      NEXT(3);
   op35: { /* listuple type size lst to */
      word type = immval(A0);
      word size = immval(A1);
      word *lst = (word *)A2;
      word *ob;
      allocate(size+1, ob);
      A3 = (word) ob;
      *ob++ = make_header(size+1, type);
      while(size--) {
         assert(pairp(lst), lst, 35);
         *ob++ = lst[1];
         lst = (word *) lst[2];
      }
      NEXT(4); }
   op36: { /* size o r */
      word *ob = (word *)A0;
      A1 = immediatep(ob) ? IFALSE : F(hdrsize(*ob) - 1);
      NEXT(2); }
   op37: /* lraw lst type r (FIXME: alloc amount testing compiler pass not in place yet) */
      A2 = prim_lraw(A0, immval(A1));
      NEXT(3);
   op38: { /* fx+ a b r o, types prechecked, signs ignored, assume fixnumbits+1 fits to machine word */
      word res = immval(A0) + immval(A1);
      A3 = BOOL(res & (1 << FBITS));
      A2 = F(res & FMAX);
      NEXT(4); }
   op39: { /* fx* a b l h */
      uint64_t res = (uint64_t)immval(A0) * (uint64_t)immval(A1);
      A2 = F(res & FMAX);
      A3 = F((res >> FBITS) & FMAX);
      NEXT(4); }
   op40: { /* fx- a b r u, args prechecked, signs ignored */
      word r = (immval(A0) | (1 << FBITS)) - immval(A1);
      A3 = r & (1 << FBITS) ? IFALSE : ITRUE;
      A2 = F(r & FMAX);
      NEXT(4); }
   op41: { /* red? node r (has highest type bit?) */
      word *node = (word *)A0;
      A1 = BOOL(allocp(node) && ((*node)&(FFRED<<TPOS)));
      NEXT(2); }
   op42: /* mkblack l k v r t */
      A4 = prim_mkff(TFF, A0, A1, A2, A3);
      NEXT(5);
   op43: /* mkred l k v r t */
      A4 = prim_mkff(TFF | FFRED, A0, A1, A2, A3);
      NEXT(5);
   op44: /* less a b r */
      A2 = prim_less(A0, A1);
      NEXT(3);
   op45: { /* set t o v r */
      A3 = prim_set(A0, A1, A2);
      NEXT(4); }
   op46: { /* fftoggle - toggle node color */
      word *node = (word *)A0;
      word *new, h;
      assert(allocp(node), node, 46);
      new = fp;
      h = *node++;
      A1 = (word) new;
      *new++ = (h^(FFRED<<TPOS));
      switch(hdrsize(h)) {
         case 5:  *new++ = *node++;
         case 4:  *new++ = *node++;
         default: *new++ = *node++;
                  *new++ = *node++; }
      fp = new;
      NEXT(2); }
   op47: /* ref t o r */ /* fixme: deprecate this later */
      A2 = prim_ref(A0, A1);
      NEXT(3);
   op48: { /* refb t o r */ /* todo: merge with ref, though 0-based  */
      A2 = prim_refb(A0, immval(A1));
      NEXT(3); }
   op49: { /* withff node l k v r */
      word hdr, *ob = (word *)A0;
      hdr = *ob++;
      A2 = *ob++; /* key */
      A3 = *ob++; /* value */
      switch(hdrsize(hdr)) {
         case 3: A1 = A4 = IEMPTY; break;
         case 4:
            if (hdr & (1 << TPOS)) { /* has right? */
               A1 = IEMPTY; A4 = *ob;
            } else {
               A1 = *ob; A4 = IEMPTY;
            }
            break;
         default:
            A1 = *ob++;
            A4 = *ob;
      }
      NEXT(5); }
   op50: { /* run thunk quantum */
      word hdr;
      ob = (word *)A0;
      R[0] = R[3];
      ticker = bank ? bank : immval(A1);
      bank = 0;
      assert(allocp(ob), ob, 50);
      hdr = *ob;
      if (is_type(hdr, TTHREAD)) {
         int pos = hdrsize(hdr) - 1;
         word code = ob[pos];
         acc = pos - 3;
         while(--pos) { R[pos] = ob[pos]; }
         ip = (unsigned char *)code + W;
      } else {
         /* call a thunk with terminal continuation */
         R[3] = IHALT; /* exit via R0 when the time comes */
         acc = 1;
         goto apply;
      }
      NEXT(0); }
   op51: { /* cons a b r */
      A2 = cons(A0, A1);
      NEXT(3); }
   op52: { /* car a r */
      word *ob = (word *)A0;
      assert(pairp(ob), ob, 52);
      A1 = ob[1];
      NEXT(2); }
   op53: { /* cdr a r */
      word *ob = (word *)A0;
      assert(pairp(ob), ob, 53);
      A1 = ob[2];
      NEXT(2); }
   op54: /* eq a b r */
      A2 = BOOL(A0 == A1);
      NEXT(3);
   op55: /* band a b r, prechecked */
      A2 = A0 & A1;
      NEXT(3);
   op56: /* bor a b r, prechecked */
      A2 = A0 | A1;
      NEXT(3);
   op57: /* bxor a b r, prechecked */
      A2 = A0 ^ (A1 & (FMAX << IPOS)); /* inherit A0's type info */
      NEXT(3);
   op58: { /* fx>> a b hi lo */
      uint64_t r = (uint64_t)immval(A0) << (FBITS - immval(A1));
      A2 = F(r>>FBITS);
      A3 = F(r&FMAX);
      NEXT(4); }
   op59: { /* fx<< a b hi lo */
      uint64_t res = (uint64_t)immval(A0) << immval(A1);
      A2 = F(res>>FBITS);
      A3 = F(res&FMAX);
      NEXT(4); }
   op60: /* unused */
      error(256, F(60), IFALSE);
   op61: /* clock <secs> <ticks> */ { /* fixme: sys */
      struct timeval tp;
      word *ob;
      allocate(6, ob); /* space for 32-bit bignum - [NUM hi [NUM lo null]] */
      ob[0] = ob[3] = NUMHDR;
      A0 = (word) (ob + 3);
      ob[2] = INULL;
      ob[5] = (word) ob;
      gettimeofday(&tp, NULL);
      A1 = F(tp.tv_usec / 1000);
      ob[1] = F(tp.tv_sec >> FBITS);
      ob[4] = F(tp.tv_sec & FMAX);
      NEXT(2); }
   op62: /* set-ticker <val> <to> -> old ticker value */ /* fixme: sys */
      A1 = F(ticker & FMAX);
      ticker = immval(A0);
      NEXT(2);
   op63: { /* sys-prim op arg1 arg2 arg3 r1 */
      A4 = prim_sys(immval(A0), A1, A2, A3);
      NEXT(5); }

super_dispatch: /* run macro instructions */
   switch(op) {
/* AUTOGENERATED INSTRUCTIONS */
      default:
         error(258, F(op), ITRUE);
   }
   goto apply;

invoke_mcp: /* R4-R6 set, set R3=cont and R4=syscall and call mcp */
   ob = (word *) R[0];
   R[0] = IFALSE;
   R[3] = F(3);
   if (allocp(ob)) {
      acc = 4;
      goto apply;
   }
   return 1; /* no mcp to handle error (fail in it?), so nonzero exit */
}

/* Initial FASL image decoding */

word get_nat() {
   word result = 0;
   word new, i;
   do {
      i = *hp++;
      new = result << 7;
      if (result != (new >> 7)) exit(9); /* overflow kills */
      result = new + (i & 127);
   } while (i & 128);
   return result;
}

word *get_field(word *ptrs, int pos) {
   if (0 == *hp) {
      byte type;
      word val;
      hp++;
      type = *hp++;
      val = make_immediate(get_nat(), type);
      *fp++ = val;
   } else {
      word diff = get_nat();
      if (ptrs != NULL) *fp++ = ptrs[pos-diff];
   }
   return fp;
}

word *get_obj(word *ptrs, int me) {
   int type, size;
   if(ptrs != NULL) ptrs[me] = (word) fp;
   switch(*hp++) { /* todo: adding type information here would reduce fasl and executable size */
      case 1: {
         type = *hp++;
         size = get_nat();
         *fp++ = make_header(size+1, type); /* +1 to include header in size */
         while(size--) { fp = get_field(ptrs, me); }
         break; }
      case 2: {
         int bytes, pads;
         byte *wp;
         type = *hp++ & 31; /* low 5 bits, the others are pads */
         bytes = get_nat();
         size = OBJWORDS(bytes);
         pads = (size-1)*W - bytes;
         *fp++ = make_raw_header(size, type, pads);
         wp = (byte *) fp;
         while (bytes--) { *wp++ = *hp++; };
         while (pads--) { *wp++ = 0; };
         fp = (word *) wp;
         break; }
      default: exit(42);
   }
   return fp;
}

/* dry run fasl decode - just compute sizes */
void get_obj_metrics(int *rwords, int *rnobjs) {
   int size;
   switch(*hp++) {
      case 1:
         hp++;
         size = get_nat();
         *rnobjs += 1;
         *rwords += size;
         while(size--) {
            if (0 == *hp)
               hp += 2;
            get_nat();
         }
         break;
      case 2:
         hp++;
         size = get_nat();
         *rnobjs += 1;
         *rwords += OBJWORDS(size);
         hp += size;
         break;
      default:
         exit(42);
   }
}

/* count number of objects and measure heap size */
void heap_metrics(int *rwords, int *rnobjs) {
   byte *hp_start = hp;
   while(*hp != 0)
      get_obj_metrics(rwords, rnobjs);
   hp = hp_start;
}

byte *read_heap(char *path) {
   struct stat st;
   int fd, pos = 0;
   if(stat(path, &st)) exit(1);
   hp = realloc(NULL, st.st_size);
   if (hp == NULL) exit(2);
   fd = open(path, O_RDONLY);
   if (fd < 0) exit(3);
   while(pos < st.st_size) {
      int n = read(fd, hp+pos, st.st_size-pos);
      if (n < 0) exit(4);
      pos += n;
   }
   close(fd);
   return hp;
}

/* find a fasl image source to *hp or exit */
void find_heap(int *nargs, char ***argv, int *nobjs, int *nwords) {
   file_heap = NULL;
   if ((word)heap == 0) {
      /* if no preloaded heap, try to load it from first vm arg */
      if (*nargs < 2) exit(1);
      file_heap = read_heap((*argv)[1]);
      if(*hp == '#') {
         while(*hp++ != '\n');
      }
      *nargs -= 1;
      *argv += 1;
   } else {
      hp = (byte *) &heap; /* builtin heap */
   }
   heap_metrics(nwords, nobjs);
}

word *decode_fasl(int nobjs) {
   word *ptrs;
   word *entry;
   int pos = 0;
   allocate(nobjs + 1, ptrs);
   while(pos < nobjs) {
      if (fp >= memend) { /* bug */
         exit(1);
      }
      fp = get_obj(ptrs, pos);
      pos++;
   }
   entry = (word *) ptrs[pos - 1];
   ptrs[0] = make_raw_header(nobjs + 1, 0, 0);
   return entry;
}

word *load_heap(int nobjs) {
   word *entry = decode_fasl(nobjs);
   if (file_heap != NULL)
      free(file_heap);
   return entry;
}

void setup(int nwords, int nobjs) {
   tcgetattr(0, &tsettings);
   state = IFALSE;
   set_signal_handler();
   max_heap_mb = W == 4 ? 4096 : 65535;
   nwords += nobjs + INITCELLS;
   memstart = genstart = fp = realloc(NULL, (nwords + MEMPAD) * W);
   if (memstart == NULL)
      exit(4);
   memend = memstart + nwords - MEMPAD;
}

int main(int nargs, char **argv) {
   word *prog;
   int rval, nobjs=0, nwords=0;
   find_heap(&nargs, &argv, &nobjs, &nwords);
   setup(nwords, nobjs);
   prog = load_heap(nobjs);
   rval = vm(prog, (word *)onum((word)argv, 0));
   setdown();
   if (fixnump(rval)) {
      int n = immval(rval);
      if (!(n & ~127))
         return n;
   }
   return 127;
}
