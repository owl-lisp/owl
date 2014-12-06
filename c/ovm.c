/* Owl Lisp runtime */

#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <inttypes.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <string.h>


/*** Portability Issues ***/

#ifdef WIN32 
#include <winsock2.h>
#include <ws2tcpip.h>
#include <conio.h>
#include <windows.h>
typedef unsigned long in_addr_t;
#define EWOULDBLOCK WSAEWOULDBLOCK
#else
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/wait.h>
#ifndef O_BINARY
#define O_BINARY 0
#endif
#endif

#ifdef __gnu_linux__
#ifndef NO_SECCOMP
#include <sys/prctl.h>
#include <sys/syscall.h>
/* normal exit() segfaults in seccomp */
#define EXIT(n) syscall(__NR_exit, n); exit(n)
#else
#define EXIT(n) exit(n)
#endif
#else
#define EXIT(n) exit(n)
#endif

typedef uintptr_t word;

#ifdef _LP64
typedef int64_t   wdiff;
#else
typedef int32_t   wdiff;
#endif


/*** Macros ***/

#define IPOS                        8 /* offset of immediate payload */
#define SPOS                        16 /* offset of size bits in header immediate values */
#define TPOS                        2  /* offset of type bits in header */
#define V(ob)                       *((word *) (ob))
#define W                           sizeof(word)
#define NWORDS                      1024*1024*8    /* static malloc'd heap size if used as a library */
#define FBITS                       24             /* bits in fixnum, on the way to 24 and beyond */
#define FMAX                        ((1<<FBITS)-1) /* maximum fixnum (and most negative fixnum) */
#define MAXOBJ                      0xffff         /* max words in tuple including header */
#define RAWBIT                      2048
#define make_immediate(value, type) (((value) << IPOS) | ((type) << TPOS) | 2)
#define make_header(size, type)     (((size) << SPOS) | ((type) << TPOS) | 2)
#define make_raw_header(s, t, p)    (((s) << SPOS) | ((t) << TPOS) | (RAWBIT|2) | ((p) << 8))
#define F(val)                      (((val) << IPOS) | 2) 
#define BOOL(cval)                  ((cval) ? ITRUE : IFALSE)
#define fixval(desc)                ((desc) >> IPOS)
#define fixnump(desc)               (((desc)&255) == 2)
#define fliptag(ptr)                ((word)ptr^2) /* make a pointer look like some (usually bad) immediate object */
#define NR                          190 /* fixme, should be ~32, see n-registers in register.scm */
#define header(x)                   *(word *x)
#define imm_type(x)                 (((x) >> TPOS) & 63)
#define imm_val(x)                  ((x) >> IPOS)
#define hdrsize(x)                  ((((word)x) >> SPOS) & MAXOBJ)
#define immediatep(x)               (((word)x)&2)
#define allocp(x)                   (!immediatep(x))
#define rawp(hdr)                   ((hdr)&RAWBIT)
#define NEXT(n)                     ip += n; op = *ip++; goto main_dispatch /* default NEXT, smaller vm */
#define NEXT_ALT(n)                 ip += n; op = *ip++; EXEC /* more branch predictor friendly, bigger vm */
#define PAIRHDR                     make_header(3,1)
#define NUMHDR                      make_header(3,40) /* <- on the way to 40, see type-int+ in defmac.scm */
#define pairp(ob)                   (allocp(ob) && V(ob)==PAIRHDR)
#define INULL                       make_immediate(0,13)
#define IFALSE                      make_immediate(1,13)
#define ITRUE                       make_immediate(2,13)
#define IEMPTY                      make_immediate(3,13) /* empty ff */
#define IEOF                        make_immediate(4,13)
#define IHALT                       INULL /* FIXME: adde a distinct IHALT */ 
#define TTUPLE                      2
#define TTHREAD                     31
#define TFF                         24
#define FFRIGHT                     1
#define FFRED                       2
#define TBVEC                       19
#define TBYTECODE                   16
#define TPROC                       17
#define TCLOS                       18
#define cont(n)                     V((word)n&(~1))
#define flagged(n)                  (n&1)
#define flag(n)                     (((word)n)^1)
#define A0                          R[*ip]               
#define A1                          R[ip[1]]
#define A2                          R[ip[2]]
#define A3                          R[ip[3]]
#define A4                          R[ip[4]]
#define A5                          R[ip[5]]
#define G(ptr,n)                    ((word *)(ptr))[n]
#define flagged_or_raw(hdr)         (hdr&(RAWBIT|1))
#define TICKS                       10000 /* # of function calls in a thread quantum  */
#define allocate(size, to)          to = fp; fp += size;
#define error(opcode, a, b)         R[4] = F(opcode); R[5] = (word) a; R[6] = (word) b; goto invoke_mcp;
#define likely(x)                   __builtin_expect((x),1)
#define unlikely(x)                 __builtin_expect((x),0)
#define assert(exp,val,code)        if(unlikely(!(exp))) {error(code, val, ITRUE);}
#define assert_not(exp,val,code)    if(unlikely(exp)) {error(code, val, ITRUE);}
#define OGOTO(f,n); ob = (word *)   R[f]; acc = n; goto apply
#define RET(n)                      ob=(word *)R[3]; R[3] = R[n]; acc = 1; goto apply
#define MEMPAD                      (NR+2)*8 /* space at end of heap for starting GC */
#define MINGEN                      1024*32  /* minimum generation size before doing full GC  */
#define INITCELLS                   1000
#define OCLOSE(proctype)            { word size = *ip++, tmp; word *ob; allocate(size, ob); tmp = R[*ip++]; tmp = ((word *) tmp)[*ip++]; *ob = make_header(size, proctype); ob[1] = tmp; tmp = 2; while(tmp != size) { ob[tmp++] = R[*ip++]; } R[*ip++] = (word) ob; }
#define CLOSE1(proctype)            { word size = *ip++, tmp; word *ob; allocate(size, ob); tmp = R[1]; tmp = ((word *) tmp)[*ip++]; *ob = make_header(size, proctype); ob[1] = tmp; tmp = 2; while(tmp != size) { ob[tmp++] = R[*ip++]; } R[*ip++] = (word) ob; }
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

/* memstart <= genstart <= memend */
static word *genstart;
static word *memstart;
static word *memend;
static word max_heap_mb; /* max heap size in MB */
static int breaked;      /* set in signal handler, passed over to owl in thread switch */
unsigned char *hp;       /* heap pointer when loading heap */
static int seccompp;     /* are we in seccomp? */
static unsigned long seccomp_time; /* virtual time within seccomp sandbox in ms */
static word *fp;
int usegc;
int slice;

word vm();
void exit(int rval);
void *realloc(void *ptr, size_t size);
void *malloc(size_t size);
void free(void *ptr);
char *getenv(const char *name);
DIR *opendir(const char *name);
DIR *fdopendir(int fd);
pid_t fork(void);
pid_t waitpid(pid_t pid, int *status, int options);
int chdir(const char *path);
#ifndef WIN32 
int execv(const char *path, char *const argv[]);
#endif

/*** Garbage Collector, based on "Efficient Garbage Compaction Algorithm" by Johannes Martin (1982) ***/

static __inline__ void rev(word pos) {
   word val = V(pos);
   word next = cont(val);
   *(word *) pos = next;
   cont(val) = (val&1)^(pos|1);
}

static __inline__ word *chase(word *pos) {
   word val = cont(pos);
   while (allocp(val) && flagged(val)) {
      pos = (word *) val;
      val = cont(pos);
   }
   return pos;
}

static void mark(word *pos, word *end) {
   while (pos != end) {
      word val = *pos;
      if (allocp(val) && val >= ((word) genstart)) { 
         if (flagged(val)) {
            pos = ((word *) flag(chase((word *) val))) - 1;
         } else {
            word hdr = V(val);
            rev((word) pos);
            if (flagged_or_raw(hdr)) {
               pos--;
            } else {
               pos = ((word *) val) + (hdrsize(hdr)-1);
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
      if (flagged(*old)) {
         word h;
         *new = *old;
         while (flagged(*new)) {
            rev((word) new);
            if (immediatep(*new) && flagged(*new)) {
               *new = flag(*new);
            }
         }
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
         old += hdrsize(*old);
      }
   }
   return new; 
}

void fix_pointers(word *pos, wdiff delta, word *end) {
   while(1) {
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

/* n-cells-wanted → heap-delta (to be added to pointers), updates memstart and memend  */
wdiff adjust_heap(int cells) {
   /* add new realloc + heap fixer here later */
   word *old = memstart;
   word nwords = memend - memstart + MEMPAD; /* MEMPAD is after memend */
   word new_words = nwords + ((cells > 0xffffff) ? 0xffffff : cells); /* limit heap growth speed  */
   if (!usegc) { /* only run when the vm is running (temp) */
      return 0;
   }
   if (seccompp) /* realloc is not allowed within seccomp */
      return 0;
   if (((cells > 0) && (new_words*W < nwords*W)) || ((cells < 0) && (new_words*W > nwords*W)))
       return 0; /* don't try to adjust heap if the size_t would overflow in realloc */
   memstart = realloc(memstart, new_words*W);
   if (memstart == old) { /* whee, no heap slide \o/ */
      memend = memstart + new_words - MEMPAD; /* leave MEMPAD words alone */
      return 0;
   } else if (memstart) { /* d'oh! we need to O(n) all the pointers... */
      wdiff delta = (word)memstart - (word)old;
      memend = memstart + new_words - MEMPAD; /* leave MEMPAD words alone */
      fix_pointers(memstart, delta, memend);
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
   int nfree;
   fp = regs + hdrsize(*regs);
   root = fp+1;
   *root = (word) regs;
   memend = fp;
   mark(root, fp);
   fp = compact();
   regs = (word *) *root;
   memend = realend;
   nfree = (word)memend - (word)regs;
   if (genstart == memstart) {
      word heapsize = (word) memend - (word) memstart;
      word nused = heapsize - nfree;
      if ((heapsize/(1024*1024)) > max_heap_mb) { 
         breaked |= 8; /* will be passed over to mcp at thread switch*/
      }
      nfree -= size*W + MEMPAD;   /* how much really could be snipped off */
      if (nfree < (heapsize / 10) || nfree < 0) {
         /* increase heap size if less than 10% is free by ~10% of heap size (growth usually implies more growth) */
         regs[hdrsize(*regs)] = 0; /* use an invalid descriptor to denote end live heap data  */
         regs = (word *) ((word)regs + adjust_heap(size*W + nused/10 + 4096));
         nfree = memend - regs;
         if (nfree <= size) {
            breaked |= 8; /* will be passed over to mcp at thread switch. may cause owl<->gc loop if handled poorly on lisp side! */
         }
      } else if (nfree > (heapsize/5)) {
         /* decrease heap size if more than 20% is free by 10% of the free space */
         int dec = -(nfree/10);
         int new = nfree - dec;
         if (new > size*W*2 + MEMPAD) {
            regs[hdrsize(*regs)] = 0; /* as above */
            regs = (word *) ((word)regs + adjust_heap(dec+MEMPAD*W));
            heapsize = (word) memend - (word) memstart; 
            nfree = (word) memend - (word) regs; 
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

void set_blocking(int sock, int blockp) {
#ifdef WIN32
   unsigned long flags = 1;
   if(sock>3) { /* stdin is read differently, out&err block */
      ioctlsocket(sock, FIONBIO, &flags);
   }
#else
   fcntl(sock, F_SETFL, (blockp?:O_NONBLOCK));
#endif
}

void signal_handler(int signal) {
#ifndef WIN32
   switch(signal) {
      case SIGINT: 
         breaked |= 2; break;
      case SIGPIPE: break; /* can cause loop when reporting errors */
      default: 
         // printf("vm: signal %d\n", signal);
         breaked |= 4;
   }
#endif
}

/* small functions defined locally after hitting some portability issues */
static void bytecopy(char *from, char *to, int n) { while(n--) *to++ = *from++; }
static void wordcopy(word *from, word *to, int n) { while(n--) *to++ = *from++; }

unsigned int lenn(char *pos, unsigned int max) { /* added here, strnlen was missing in win32 compile */
   unsigned int p = 0;
   while(p < max && *pos++) p++;
   return p;
}

/* list length, no overflow or valid termination checks */
int llen(word *ptr) {
   int len = 0;
   while(allocp(ptr) && *ptr == PAIRHDR) {
      len++;
      ptr = (word *) ptr[2];
   }
   return len;
}

void set_signal_handler() {
#ifndef WIN32
   struct sigaction sa;
   sa.sa_handler = signal_handler;
   sigemptyset(&sa.sa_mask);
   sa.sa_flags = SA_RESTART;
   sigaction(SIGINT, &sa, NULL);
   sigaction(SIGPIPE, &sa, NULL);
#endif
}

/* make a byte vector object to hold len bytes (compute size, advance fp, set padding count) */
static word *mkbvec(int len, int type) {
   int nwords = (len/W) + ((len % W) ? 2 : 1);
   int pads = (nwords-1)*W - len;
   word *ob = fp;
   fp += nwords;
   *ob = make_raw_header(nwords, type, pads);
   return ob;
}

/* map a null or C-string to False, Null or owl-string, false being null or too large string */
word strp2owl(char *sp) {
   int len;
   word *res;
   if (!sp) return IFALSE;
   len = lenn(sp, FMAX+1);
   if (len == FMAX+1) return INULL; /* can't touch this */
   res = mkbvec(len, TBVEC); /* make a bvec instead of a string since we don't know the encoding */
   bytecopy(sp, ((char *)res)+W, len);
   return (word)res;
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
      unsigned char type;
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
         unsigned char *wp;
         type = *hp++ & 31; /* low 5 bits, the others are pads */
         bytes = get_nat();
         size = ((bytes % W) == 0) ? (bytes/W)+1 : (bytes/W) + 2;
         pads = (size-1)*W - bytes;
         *fp++ = make_raw_header(size, type, pads);
         wp = (unsigned char *) fp;
         while (bytes--) { *wp++ = *hp++; };
         while (pads--) { *wp++ = 0; };
         fp = (word *) wp;
         break; }
      default: puts("bad object in heap"); exit(42);
   }
   return fp;
}

/* count number of objects and measure heap size */
int count_objs(word *words) {
   word *orig_fp = fp;
   word nwords = 0;
   unsigned char *orig_hp = hp;
   int n = 0;
   while(*hp != 0) {
      get_obj(NULL, 0); /* dry run just to count the objects */
      nwords += ((word)fp - (word)orig_fp)/W;
      fp = orig_fp;
      n++;
   }
   *words = nwords;
   hp = orig_hp;
   return n;
}

unsigned char *load_heap(char *path) { 
   struct stat st;
   int fd, pos = 0;
   if(stat(path, &st)) exit(1);
   hp = realloc(NULL, st.st_size);
   if (hp == NULL) exit(2);
   fd = open(path, O_RDONLY | O_BINARY);
   if (fd < 0) exit(3);
   while(pos < st.st_size) {
      int n = read(fd, hp+pos, st.st_size-pos);
      if (n < 0) exit(4);
      pos += n;
   }
   close(fd);
   return hp;
}


/*** Primops called from VM and generated C-code ***/

static word prim_connect(word *host, word port) {
   int sock;
   unsigned char *ip = ((unsigned char *) host) + W;
   unsigned long ipfull;
   struct sockaddr_in addr;
   port = fixval(port);
   if (!allocp(host))  /* bad host type */
      return IFALSE;
   if ((sock = socket(PF_INET, SOCK_STREAM, 0)) == -1)
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
   set_blocking(sock,0);
   return F(sock);
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
         case 3: return def; 
         case 4:
            if (key < this) {
               ff = (word *) ((hdr & (1 << TPOS)) ? IEMPTY : ff[3]);
            } else {
               ff = (word *) ((hdr & (1 << TPOS)) ? ff[3] : IEMPTY);
            }
            break;
         default:
            ff = (word *) ((key < this) ? ff[3] : ff[4]);
      }
   }
   return def;
}

static word prim_cast(word *ob, int type) {
   if (immediatep((word)ob)) {
      return make_immediate(imm_val((word)ob), type);
   } else { /* make a clone of more desired type */
      word hdr = *ob++;
      int size = hdrsize(hdr);
      word *new, *res; /* <- could also write directly using *fp++ */
      allocate(size, new);
      res = new;
      /* (hdr & 0b...11111111111111111111100000000111) | tttttttt000 */
      //*new++ = (hdr&(~2040))|(type<<TPOS);
      *new++ = (hdr&(~252))|(type<<TPOS); /* <- hardcoded ...111100000011 */
      wordcopy(ob,new,size-1);
      return (word)res;
   }
}

static int prim_refb(word pword, int pos) {
   word *ob = (word *) pword;
   word hdr, hsize;
   if (immediatep(ob))
      return -1;
   hdr = *ob;
   hsize = ((hdrsize(hdr)-1)*W) - ((hdr>>8)&7); /* bytes - pads */ 
   if (pos >= hsize) 
      return IFALSE;
   return F(((unsigned char *) ob)[pos+W]);
}

static word prim_ref(word pword, word pos)  {
   word *ob = (word *) pword;
   word hdr, size;
   pos = fixval(pos);
   if(immediatep(ob)) { return IFALSE; }
   hdr = *ob;
   if (rawp(hdr)) { /* raw data is #[hdrbyte{W} b0 .. bn 0{0,W-1}] */ 
      size = ((hdrsize(hdr)-1)*W) - ((hdr>>8)&7);
      if (pos >= size) { return IFALSE; }
      return F(((unsigned char *) ob)[pos+W]);
   }
   size = hdrsize(hdr);
   if (!pos || size <= pos) /* tuples are indexed from 1 (probably later 0-255)*/
      return IFALSE;
   return ob[pos];
}

static word prim_set(word wptr, word pos, word val) {
   word *ob = (word *) wptr;
   word hdr;
   word *new;
   int p = 0;
   pos = fixval(pos);
   if(immediatep(ob)) { return IFALSE; }
   hdr = *ob;
   if (rawp(hdr) || hdrsize(hdr) < pos) { return IFALSE; }
   hdr = hdrsize(hdr);
   allocate(hdr, new);
   while(p <= hdr) {
      new[p] = (pos == p && p) ? val : ob[p];
      p++;
   }
   return (word) new;
}

/* system- and io primops */
static word prim_sys(int op, word a, word b, word c) {
   switch(op) {
      case 0: { /* 0 fsend fd buff len r → n if wrote n, 0 if busy, False if error (argument or write) */
         int fd = fixval(a);
         word *buff = (word *) b;
         int wrote, size, len = fixval(c);
         if (immediatep(buff)) return IFALSE;
         size = (hdrsize(*buff)-1)*W;
         if (len > size) return IFALSE;
         wrote = write(fd, ((char *)buff)+W, len);
         if (wrote > 0) return F(wrote);
         if (errno == EAGAIN || errno == EWOULDBLOCK) return F(0);
         return IFALSE; }
      case 1: { /* 1 = fopen <str> <mode> <to> */
         char *path = (char *) a;
         int mode = fixval(b);
         int val;
         struct stat sb;
         if (!(allocp(path) && imm_type(*path) == 3))
            return IFALSE;
         mode |= O_BINARY | ((mode > 0) ? O_CREAT | O_TRUNC : 0);
         val = open(((char *) path) + W, mode,(S_IRUSR|S_IWUSR));
         if (val < 0 || fstat(val, &sb) == -1 || sb.st_mode & S_IFDIR) {
            close(val);
            return IFALSE;
         }
         set_blocking(val,0);
         return F(val); }
      case 2: 
         return close(fixval(a)) ? IFALSE : ITRUE;
      case 3: { /* 3 = sopen port -> False | fd  */
         int port = fixval(a);
         int s;
         int opt = 1; /* TRUE */
         struct sockaddr_in myaddr;
         myaddr.sin_family = AF_INET;
         myaddr.sin_port = htons(port);
         myaddr.sin_addr.s_addr = INADDR_ANY;
         s = socket(AF_INET, SOCK_STREAM, 0);
#ifndef WIN32
         if (s < 0) return IFALSE;
#else
	 if (s == INVALID_SOCKET) return IFALSE;
#endif
         if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&opt, sizeof(opt)) \
             || bind(s, (struct sockaddr *) &myaddr, sizeof(myaddr)) != 0 \
             || listen(s, 5) != 0) {
            close(s);
            return IFALSE;
         }
         set_blocking(s,0);
         return F(s); }
      case 4: { /* 4 = accept port -> rval=False|(ip . fd) */
         int sock = fixval(a);
         struct sockaddr_in addr;
         socklen_t len = sizeof(addr);
         int fd;
         word *pair;
         char *ipa;
         fd = accept(sock, (struct sockaddr *)&addr, &len);
         if (fd < 0) return IFALSE;
         set_blocking(fd,0);
         ipa = (char *) &addr.sin_addr;
         *fp = make_raw_header(2, TBVEC, 4%W);
         bytecopy(ipa, ((char *) fp) + W, 4);
         fp[2] = PAIRHDR;
         fp[3] = (word) fp;
         fp[4] = F(fd);
         pair = fp+2;
         fp += 5;
         return (word)pair; }
      case 5: { /* fread fd max -> obj | eof | F (read error) | T (would block) */
         word fd = fixval(a);
         word max = fixval(b); 
         word *res;
         int n, nwords = (max/W) + 2;
         allocate(nwords, res);
#ifndef WIN32
         n = read(fd, ((char *) res) + W, max);
#else
         if (fd == 0) { /* windows stdin in special apparently  */
            if(!_isatty(0) || _kbhit()) { /* we don't get hit by kb in pipe */
               n = read(fd, ((char *) res) + W, max);
            } else {
               n = -1;
               errno = EAGAIN;
            }

         } else {
            n = read(fd, ((char *) res) + W, max);
         }
#endif
         if (n > 0) { /* got some bytes */
            word read_nwords = (n/W) + ((n%W) ? 2 : 1); 
            int pads = (read_nwords-1)*W - n;
            fp = res + read_nwords;
            *res = make_raw_header(read_nwords, TBVEC, pads);
            return (word)res;
         }
         fp = res;
         if (n == 0)
            return IEOF;
         return BOOL(errno == EAGAIN || errno == EWOULDBLOCK); }
      case 6:
         EXIT(fixval(a)); /* stop the press */
      case 7: /* set memory limit (in mb) */
         max_heap_mb = fixval(a);
         return a;
      case 8: /* get machine word size (in bytes) */
         return F(W);
      case 9: /* get memory limit (in mb) */
         return F(max_heap_mb);
      case 10: /* enter linux seccomp mode */
#ifdef __gnu_linux__ 
#ifndef NO_SECCOMP
         if (seccompp) /* a true value, but different to signal that we're already in seccomp */
            return INULL;  
         seccomp_time = 1000 * time(NULL); /* no time calls are allowed from seccomp, so start emulating a time if success */
#ifdef PR_SET_SECCOMP
         if (prctl(PR_SET_SECCOMP,1) != -1) { /* true if no problem going seccomp */
            seccompp = 1;
            return ITRUE;
         }
#endif
#endif
#endif
         return IFALSE; /* seccomp not supported in current repl */
      /* dirops only to be used via exposed functions */
      case 11: { /* sys-opendir path _ _ -> False | dirobjptr */
         char *path = W + (char *) a; /* skip header */
         DIR *dirp = opendir(path);
         if(!dirp) return IFALSE;
         return fliptag(dirp); }
      case 12: { /* sys-readdir dirp _ _ -> bvec | eof | False */
         DIR *dirp = (DIR *)fliptag(a);
         word *res;
         unsigned int len;
         struct dirent *dire = readdir(dirp);
         if (!dire) return IEOF; /* eof at end of dir stream */
         len = lenn(dire->d_name, FMAX+1);
         if (len == FMAX+1) return IFALSE; /* false for errors, like too long file names */
         res = mkbvec(len, 3); /* make a fake raw string (OS may not use valid UTF-8) */
         bytecopy((char *)&dire->d_name, (char *) (res + 1), len); /* *no* terminating null, this is an owl bvec */
         return (word)res; }
      case 13: /* sys-closedir dirp _ _ -> ITRUE */
         closedir((DIR *)fliptag(a));
         return ITRUE;
      case 14: { /* set-ticks n _ _ -> old */
         word old = F(slice); 
         slice = fixval(a);
         return old; }
      case 15: { /* 0 fsocksend fd buff len r → n if wrote n, 0 if busy, False if error (argument or write) */
         int fd = fixval(a);
         word *buff = (word *) b;
         int wrote, size, len = fixval(c);
         if (immediatep(buff)) return IFALSE;
         size = (hdrsize(*buff)-1)*W;
         if (len > size) return IFALSE;
         wrote = send(fd, ((char *)buff)+W, len, 0); /* <- no MSG_DONTWAIT in win32 */
         if (wrote > 0) return F(wrote);
         if (errno == EAGAIN || errno == EWOULDBLOCK) return F(0);
         return IFALSE; }
      case 16: { /* getenv <owl-raw-bvec-or-ascii-leaf-string> */
         char *name = (char *)a;
         if (!allocp(name)) return IFALSE;
         return strp2owl(getenv(name + W)); }
      case 17: { /* exec[v] path argl ret */
         char *path = ((char *) a) + W;
         int nargs = llen((word *)b);
         char **args = malloc((nargs+1) * sizeof(char *));
         char **argp = args;
#ifndef WIN32 
         if (args == NULL) 
            return IFALSE;
         while(nargs--) {
            *argp++ = ((char *) ((word *) b)[1]) + W;
            b = ((word *) b)[2];
         }
         *argp = NULL;
         set_blocking(0,1); /* try to return stdio to blocking mode */
         set_blocking(1,1); /* warning, other file descriptors will stay in nonblocking mode */
         set_blocking(2,1);
         execv(path, args); /* may return -1 and set errno */
         set_blocking(0,0); /* exec failed, back to nonblocking io for owl */
         set_blocking(1,0);
         set_blocking(2,0);
#endif
         return IFALSE; }
      case 20: { /* chdir path res */
         char *path = ((char *)a) + W;
         if (chdir(path) < 0)
            return IFALSE;
         return ITRUE; }
#ifndef WIN32
      case 19: { /* wait <pid> <respair> _ */
         pid_t pid = (a == IFALSE) ? -1 : fixval(a);
         int status;
         word *r = (word *) b;
         pid = waitpid(pid, &status, WNOHANG|WUNTRACED|WCONTINUED);
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
         } else if (WIFCONTINUED(status)) {
            r[1] = F(4);
            r[2] = F(1);
         } else {
            fprintf(stderr, "vm: unexpected process exit status: %d\n", status);
            r = (word *)IFALSE;
         }
         return (word)r; }
      case 18: { /* fork ret → #false=failed, fixnum=ok we're in parent process, #true=ok we're in child process */
         pid_t pid = fork();
         if (pid == -1) /* fork failed */
            return IFALSE;
         if (pid == 0) /* we're in child, return true */
            return ITRUE;
         if ((int)pid > FMAX)
            fprintf(stderr, "vm: child pid larger than max fixnum: %d\n", pid);
         return F(pid&FMAX); }
      case 21: /* kill pid signal → fixnum */
         return (kill(fixval(a), fixval(b)) < 0) ? IFALSE : ITRUE;
#endif
      default: 
         return IFALSE;
   }
}

static word prim_lraw(word wptr, int type, word revp) {
   word *lst = (word *) wptr;
   int nwords, len = 0, pads;
   unsigned char *pos;
   word *raw, *ob;
   if (revp != IFALSE) { exit(1); } /* <- to be removed */
   ob = lst;
   while (allocp(ob) && *ob == PAIRHDR) {
      len++;
      ob = (word *) ob[2];
   }
   if ((word) ob != INULL) return IFALSE;
   if (len > FMAX) return IFALSE;
   nwords = (len/W) + ((len % W) ? 2 : 1);
   allocate(nwords, raw);
   pads = (nwords-1)*W - len; /* padding byte count, usually stored to top 3 bits */
   *raw = make_raw_header(nwords, type, pads);
   ob = lst;
   pos = ((unsigned char *) raw) + W;
   while ((word) ob != INULL) {
      *pos++ = fixval(ob[1])&255;
      ob = (word *) ob[2];
   }
   while(pads--) { *pos++ = 0; } /* clear the padding bytes */
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
         *ob = make_header(4, t|FFRIGHT); 
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

/* Load heap, convert arguments and start VM */

word boot(int nargs, char **argv) {
   int this, pos, nobjs;
   unsigned char *file_heap = NULL;
   word *entry;
   word *oargs = (word *) INULL;
   word *ptrs;
   word nwords;
   usegc = seccompp = 0;
   slice = TICKS; /* default thread slice (n calls per slice) */
   if (heap == NULL) { /* if no preloaded heap, try to load it from first arg */
      if (nargs < 2) exit(1);
      file_heap = load_heap(argv[1]);
      if(*hp == 35) { /* skip hashbang */
         while(*hp++ != 10);
      }
      nargs--; argv++; /* skip vm */
   } else {
      hp = (unsigned char *) &heap;
   }
   max_heap_mb = (W == 4) ? 4096 : 65535; /* can be set at runtime */
   memstart = genstart = fp = (word *) realloc(NULL, (INITCELLS + FMAX + MEMPAD)*W); /* at least one argument string always fits */
   if (!memstart) {
      fprintf(stderr, "Failed to allocate initial memory\n");
      exit(4);
   }
   memend = memstart + FMAX + INITCELLS - MEMPAD;
   this = nargs-1;
   usegc = 1;
   while(this >= 0) { /* build an owl string list to oargs at bottom of heap */
      char *str = argv[this];
      char *pos = str;
      int pads;
      word *tmp;
      int len = 0, size;
      while(*pos++) len++;
      if (len > FMAX) {
         puts("owl: command line argument too long");
         exit(1);
      }
      size = ((len % W) == 0) ? (len/W)+1 : (len/W) + 2;
      if ((word)fp + size >= (word)memend) {
         oargs = gc(FMAX, oargs); /* oargs points to topmost pair, may move as a result of gc */
         fp = oargs + 3;
      }
      pads = (size-1)*W - len;
      tmp = fp;
      fp += size;
      *tmp = make_raw_header(size, 3, pads);
      pos = ((char *) tmp) + W;
      while(*str) *pos++ = *str++;
      *fp = PAIRHDR;
      fp[1] = (word) tmp;
      fp[2] = (word) oargs;
      oargs = fp;
      fp += 3;
      this--;
   }
   nobjs = count_objs(&nwords);
   oargs = gc(nwords+(128*1024), oargs); /* get enough space to load the heap without triggering gc */
   fp = oargs + 3;
   ptrs = fp;
   fp += nobjs+1;
   pos = 0;
   while(pos < nobjs) { /* or until fasl stream end mark */
      if (fp >= memend) {
         puts("gc needed during heap import\n");
         exit(1);
      }
      fp = get_obj(ptrs, pos);
      pos++;
   }
   entry = (word *) ptrs[pos-1]; 
   /* disable buffering */
   setvbuf(stdin, NULL, _IONBF, 0);
   setvbuf(stdout, NULL, _IONBF, 0);
   setvbuf(stderr, NULL, _IONBF, 0);
   /* set up signal handler */
   set_signal_handler();
   set_blocking(0,0); /* change to nonblocking stdio */
   set_blocking(1,0);
   set_blocking(2,0);
   /* clear the pointers */
   /* fixme, wrong when heap has > 65534 objects */
   ptrs[0] = make_raw_header(nobjs+1,0,0);
   if (file_heap != NULL) free((void *) file_heap);
   return vm(entry, oargs);
}

word vm(word *ob, word *args) {
   unsigned char *ip;
   int bank = 0; /* ticks deposited at syscall */
   int ticker = slice; /* any initial value ok */
   unsigned short acc = 0; /* no support for >255arg functions */
   int op; /* opcode to execute */
   static word R[NR];
   word load_imms[] = {F(0), INULL, ITRUE, IFALSE};  /* for ldi and jv */
   usegc = 1; /* enble gc (later have if always evabled) */

   /* clear blank regs */
   while(acc < NR) { R[acc++] = INULL; }
   R[0] = IFALSE;
   R[3] = IHALT;
   R[4] = (word) args;
   acc = 2; /* boot always calls with 2 args*/

apply: /* apply something at ob to values in regs, or maybe switch context */

   if (likely(allocp(ob))) {
      word hdr = *ob & 4095; /* cut size out, take just header info */
      if (hdr == make_header(0,TPROC)) { /* proc */
         R[1] = (word) ob; ob = (word *) ob[1];
      } else if (hdr == make_header(0,TCLOS)) { /* clos */
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
      } else if (((hdr >> TPOS) & 63) != TBYTECODE) { /* not even code, extend bits later */
         error(259, ob, INULL);
      }
      if (unlikely(!ticker--)) goto switch_thread;
      ip = ((unsigned char *) ob) + W;
      goto invoke;
   } else if ((word)ob == IEMPTY && acc > 1) { /* ff application: (False key def) -> def */
      ob = (word *) R[3]; /* call cont */
      R[3] = (acc > 2) ? R[5] : IFALSE; /* default arg or false if none */
      acc = 1;
      goto apply;
   } else if ((word)ob == IHALT) {
      /* a tread or mcp is calling the final continuation  */
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
      return fixval(R[3]);
   } /* <- add a way to call the new vm prim table also here? */
   error(257, ob, INULL); /* not callable */

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
      R[3] = breaked ? ((breaked & 8) ? F(14) : F(10)) : F(1); /* fixme - handle also differnet signals via one handler  */
      R[4] = (word) state;
      R[5] = F(breaked);
      R[6] = IFALSE;
      acc = 4;
      breaked = 0;
      goto apply;
   }
invoke: /* nargs and regs ready, maybe gc and execute ob */
   if (((word)fp) + 1024*64 >= ((word) memend))
      //(1)  // always gc
	{
      int p = 0; 
      *fp = make_header(NR+2, 50); /* hdr r_0 .. r_(NR-1) ob */ 
      while(p < NR) { fp[p+1] = R[p]; p++; } 
      fp[p+1] = (word) ob;
      fp = gc(1024*64, fp);
      ob = (word *) fp[p+1];
      while(--p >= 0) { R[p] = fp[p+1]; }
      ip = ((unsigned char *) ob) + W;
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
   op1: {word *ob = (word *)R[*ip]; R[ip[2]] = ob[ip[1]]; NEXT(3);}
   op2: OGOTO(*ip,ip[1]); /* fixme, these macros are not used in cgen output anymore*/
   op3: OCLOSE(TCLOS); NEXT(0);
   op4: OCLOSE(TPROC); NEXT(0);
   op5: /* mov2 from1 to1 from2 to2 */
      R[ip[1]] = R[ip[0]];
      R[ip[3]] = R[ip[2]];
      NEXT(4);
   op6: CLOSE1(TCLOS); NEXT(0);
   op7: CLOSE1(TPROC); NEXT(0);
   op8: /* jlq a b o, extended jump  */
      if(R[*ip] == A1) { ip += ip[2] + (ip[3] << 8); } 
      NEXT(4); 
   op9: R[ip[1]] = R[*ip]; NEXT(2);
   op11: /* unused */
      error(11, IFALSE, IFALSE);
   op10: /* unused */
      error(10, IFALSE, IFALSE);
   op12: /* unused */
      error(12, IFALSE, IFALSE);
   op13: /* ldi{2bit what} [to] */
      R[*ip++] = load_imms[op>>6];
      NEXT(0);
   op14: R[ip[1]] = F(*ip); NEXT(2);
   op15: { /* type-byte o r <- actually sixtet */
      word ob = R[*ip++];
      if (allocp(ob)) ob = V(ob);
      R[*ip++] = F((ob>>TPOS)&63);
      NEXT(0); }
   op16: /* jv[which] a o1 a2*/
      /* FIXME, convert this to jump-const <n> comparing to make_immediate(<n>,TCONST) */
      if(R[*ip] == load_imms[op>>6]) { ip += ip[1] + (ip[2] << 8); } 
      NEXT(3); 
   op17: /* arity error */
      error(17, ob, F(acc));
   op18: /* goto-code p */
      ob = (word *) R[*ip]; /* needed in opof gc */
      acc = ip[1];
      ip = ((unsigned char *) R[*ip]) + W;
      goto invoke;
   op19: { /* goto-proc p */
      word *this = (word *) R[*ip];
      R[1] = (word) this;
      acc = ip[1];
      ob = (word *) this[1];
      ip = ((unsigned char *) ob) + W;
      goto invoke; }
   op20: { 
      int reg, arity;
      word *lst;
      if (op == 20) { /* normal apply: cont=r3, fn=r4, a0=r5, */ 
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
      } else { /* _sans_cps apply: func=r3, a0=r4, */ 
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
      while(allocp(lst) && *lst == PAIRHDR) { /* unwind argument list */
         /* FIXME: unwind only up to last register and add limited rewinding to arity check */
         if (reg > 128) { /* dummy handling for now */
            fprintf(stderr, "TOO LARGE APPLY\n");
            exit(3);
         }
         R[reg++] = lst[1];
         lst = (word *) lst[2];
         arity++;
      }
      acc = arity;
      goto apply; }
   op21: { /* goto-clos p */
      word *this = (word *) R[*ip];
      R[1] = (word) this;
      acc = ip[1];
      this = (word *) this[1];
      R[2] = (word) this;
      ob = (word *) this[1];
      ip = ((unsigned char *) ob) + W;
      goto invoke; }
   op22: { /* cast o t r */
      word *ob = (word *) R[*ip];
      word type = fixval(A1) & 63;
      A2 = prim_cast(ob, type);
      NEXT(3); }
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
   op24: /* ret val */
      ob = (word *) R[3];
      R[3] = R[*ip];
      acc = 1;
      goto apply; 
   op25: { /* jmp-nargs(>=?) a hi lo */
      int needed = *ip;
      if (acc == needed) {
         if (op & 64) /* add empty extra arg list */
            R[acc + 3] = INULL;
      } else if ((op & 64) && acc > needed) {
         word tail = INULL;  /* todo: no call overflow handling yet */
         while (acc > needed) {
            fp[0] = PAIRHDR;
            fp[1] = R[acc + 2];
            fp[2] = tail;
            tail = (word) fp;
            fp += 3;
            acc--;
         }
         R[acc + 3] = tail;
      } else {
         ip += (ip[1] << 8) | ip[2];
      }
      NEXT(3); }
   op26: { /* fxqr ah al b qh ql r, b != 0, int32 / int16 -> int32, as fixnums */
      uint64_t a = (((uint64_t) fixval(A0))<<FBITS) | fixval(A1); 
      word b = fixval(A2);
      uint64_t q;
      q = a / b;
      A3 = F(q>>FBITS);
      A4 = F(q&FMAX);
      A5 = F(a - q*b);
      NEXT(6); }
   op27: /* syscall cont op arg1 arg2 */
      ob = (word *) R[0];
      R[0] = IFALSE;
      R[3] = A1; R[4] = R[*ip]; R[5] = A2; R[6] = A3;
      acc = 4;
      if (ticker > 10) bank = ticker; /* deposit remaining ticks for return to thread */
      goto apply;
   op28: { /* sizeb obj to */
      word ob = R[*ip];
      if (immediatep(ob)) {
         A1 = IFALSE;
      } else {
         word hdr = V(ob);
         A1 = (rawp(hdr)) ? F((hdrsize(hdr)-1)*W - ((hdr >> 8) & 7)) : IFALSE;
      }
      NEXT(2); }
   op29: { /* ncons a b r */
      *fp = NUMHDR;
      fp[1] = A0;
      fp[2] = A1;
      A2 = (word) fp;
      fp += 3;
      NEXT(3); }
   op30: { /* ncar a rd */
      word *ob = (word *) R[*ip];
      assert(allocp(ob), ob, 30);
      A1 = ob[1];
      NEXT(2); }
   op31: { /* ncdr a r */
      word *ob = (word *) R[*ip];
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
   op34: { /* connect <host-ip> <port> <res> -> fd | False, via an ipv4 tcp stream */
      A2 = prim_connect((word *) A0, A1); /* fixme: remove and put to prim-sys*/
      NEXT(3); }
   op35: { /* listuple type size lst to */
      word type = fixval(R[*ip]);
      word size = fixval(A1);
      word *lst = (word *) A2;
      word *ob;
      allocate(size+1, ob);
      A3 = (word) ob;
      *ob++ = make_header(size+1, type);
      while(size--) {
         assert((allocp(lst) && *lst == PAIRHDR), lst, 35);
         *ob++ = lst[1];
         lst = (word *) lst[2];
      }
      NEXT(4); }
   op36: { /* size o r */
      word *ob = (word *) R[ip[0]];
      R[ip[1]] = (immediatep(ob)) ? IFALSE : F(hdrsize(*ob)-1);
      NEXT(2); }
   op37: { /* ms r */
#ifndef WIN32
      if (!seccompp)
         usleep(fixval(A0)*1000);
#else
      Sleep(fixval(A0));
#endif
      A1 = BOOL(errno == EINTR);
      NEXT(2); }
   op38: { /* fx+ a b r o, types prechecked, signs ignored, assume fixnumbits+1 fits to machine word */
      word res = fixval(A0) + fixval(A1);
      word low = res & FMAX;
      A3 = (res & (1 << FBITS)) ? ITRUE : IFALSE;
      A2 = F(low);
      NEXT(4); }
   op39: { /* fx* a b l h */
      uint64_t res = ((uint64_t) ((uint64_t) fixval(R[*ip])) * ((uint64_t) fixval(A1)));
      A2 = F(((word)(res&FMAX)));
      A3 = F(((word)(res>>FBITS)&FMAX));
      NEXT(4); }
   op40: { /* fx- a b r u, args prechecked, signs ignored */
      word r = (fixval(A0)|(1<<FBITS)) - fixval(A1);
      A3 = (r & (1<<FBITS)) ? IFALSE : ITRUE;
      A2 = F(r&FMAX);
      NEXT(4); }
   op41: { /* red? node r (has highest type bit?) */
      word *node = (word *) R[*ip];
      A1 = BOOL(allocp(node) && ((*node)&(FFRED<<TPOS)));
      NEXT(2); }
   op42: /* mkblack l k v r t */ 
      A4 = prim_mkff(TFF,A0,A1,A2,A3);
      NEXT(5);
   op43: /* mkred l k v r t */ 
      A4 = prim_mkff(TFF|FFRED,A0,A1,A2,A3);
      NEXT(5);
   op44: /* less a b r */
      A2 = prim_less(A0, A1);
      NEXT(3);
   op45: { /* set t o v r */
      A3 = prim_set(A0, A1, A2);
      NEXT(4); }
   op46: { /* fftoggle - toggle node color */
      word *node = (word *) R[*ip];
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
   op47:  /* ref t o r */ /* fixme: deprecate this later */
      A2 = prim_ref(A0, A1);
      NEXT(3);
   op48: { /* refb t o r */ /* todo: merge with ref, though 0-based  */
      A2 = prim_refb(A0, fixval(A1));
      NEXT(3); }
   op49: { /* withff node l k v r */
      word hdr, *ob = (word *) R[*ip];
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
   op50: { /* run thunk quantum */ /* fixme: maybe move to sys */
      word hdr;
      ob = (word *) A0;
      R[0] = R[3];
      ticker = bank ? bank : fixval(A1);
      bank = 0;
      assert(allocp(ob),ob,50);
      hdr = *ob;
      if (imm_type(hdr) == TTHREAD) {
         int pos = hdrsize(hdr) - 1;
         word code = ob[pos];
         acc = pos - 3;
         while(--pos) { R[pos] = ob[pos]; }
         ip = ((unsigned char *) code) + W;
      } else {
         /* call a thunk with terminal continuation */
         R[3] = IHALT; /* exit via R0 when the time comes */
         acc = 1;
         goto apply;
      }
      NEXT(0); }
   op51: { /* cons a b r */
      *fp = PAIRHDR;
      fp[1] = A0;
      fp[2] = A1;
      A2 = (word) fp;
      fp += 3;
      NEXT(3); }
   op52: { /* car a r */
      word *ob = (word *) R[*ip++];
      assert(pairp(ob), ob, 52);
      R[*ip++] = ob[1];
      NEXT(0); }
   op53: { /* cdr a r */
      word *ob = (word *) R[*ip++];
      assert(pairp(ob), ob, 52);
      R[*ip++] = ob[2];
      NEXT(0); }
   op54: /* eq a b r */
      A2 = BOOL(R[*ip] == A1);
      NEXT(3);
   op55: { /* band a b r, prechecked */
      word a = R[*ip];
      word b = A1;
      A2 = a & b;
      NEXT(3); }
   op56: { /* bor a b r, prechecked */
      word a = R[*ip];
      word b = A1;
      A2 = a | b;
      NEXT(3); }
   op57: { /* bxor a b r, prechecked */
      word a = R[*ip];
      word b = A1;
      A2 = a ^ (b & (FMAX << IPOS)); /* inherit a's type info */
      NEXT(3); }
   op58: { /* fx>> a b hi lo */
      uint64_t r = ((uint64_t) fixval(A0)) << (FBITS - fixval(A1));
      A2 = F(r>>FBITS);
      A3 = F(r&FMAX);
      NEXT(4); }
   op59: { /* fx<< a b hi lo */
      uint64_t res = (uint64_t) fixval(R[*ip]) << fixval(A1);
      A2 = F(res>>FBITS);
      A3 = F(res&FMAX);
      NEXT(4); }
   op60: /* lraw lst type dir r (fixme, alloc amount testing compiler pass not in place yet!) */
      A3 = prim_lraw(A0, fixval(A1), A2);
      NEXT(4);
   op61: /* clock <secs> <ticks> */ { /* fixme: sys */
      struct timeval tp;
      word *ob;
      allocate(6, ob); /* space for 32-bit bignum - [NUM hi [NUM lo null]] */
      ob[0] = ob[3] = NUMHDR; 
      A0 = (word) (ob + 3);
      ob[2] = INULL; 
      ob[5] = (word) ob;
      if (seccompp) {
         unsigned long secs = seccomp_time / 1000;
         A1 = F(seccomp_time - (secs * 1000));
         ob[1] = F(secs >> FBITS);
         ob[4] = F(secs & FMAX);
         seccomp_time += ((seccomp_time + 10) > seccomp_time) ? 10 : 0; /* virtual 10ms passes */
      } else {
         gettimeofday(&tp, NULL);
         A1 = F(tp.tv_usec / 1000);
         ob[1] = F(tp.tv_sec >> FBITS);
         ob[4] = F(tp.tv_sec & FMAX);
      }
      NEXT(2); }
   op62: /* set-ticker <val> <to> -> old ticker value */ /* fixme: sys */
      A1 = F(ticker & FMAX);
      ticker = fixval(A0);
      NEXT(2); 
   op63: { /* sys-prim op arg1 arg2 arg3 r1 */
      A4 = prim_sys(fixval(A0), A1, A2, A3);
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
   return 1; /* no mcp to handle error (fail in it?), so nonzero exit  */
}

int main(int nargs, char **argv) {
#ifndef WIN32
   return boot(nargs, argv);
#else
   WSADATA wsaData;

   // Initialize Winsock
   int sock_init = WSAStartup(MAKEWORD(2,2), &wsaData);
   if (sock_init  != 0) {
       printf("WSAStartup failed with error: %d\n", sock_init);
       return 1;
   }
   word boot_result = boot(nargs, argv);
   WSACleanup();
   return boot_result;
#endif
}

