/********************\
 * owl lisp runtime *
\********************/

/* todo: could be more convenient to have instructions fadd a b → hi lo, fsub a b → borrow a' */
/* todo: control gc from lisp side? */

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
#define O_BINARY 0
#endif

/* prim_sys requirements */
#include <dirent.h>  // opendir
DIR *opendir(const char *name);
DIR *fdopendir(int fd);
#include <string.h>
size_t strlen(const char *s);
/* sys_mirp */


#ifdef USE_SECCOMP
#include <sys/prctl.h>
#endif

void set_nonblock (int sock) {
#ifdef WIN32
   unsigned long flags = 1;
   if(sock>3) { /* stdin is handled differently, out&err block */
      ioctlsocket(sock, FIONBIO, &flags);
   }
#else
   fcntl(sock, F_SETFL, O_NONBLOCK);
#endif
}

#define word uintptr_t
#define W    sizeof(word)
#define NWORDS 1024*1024*8  /* static malloc'd heap size if used as a library */

#define V(ob) *((word *) ob)

/* memstart <= genstart <= memend */
static word *genstart;
static word *memstart;
static word *memend;
static word max_heap_mb; /* max heap size in MB */
static int breaked;      /* set in signal handler, passed over to owl in thread switch */
unsigned char *hp;       /* heap pointer when loading heap */
static int seccompp;     /* are we in seccomp? */

/* gc measuring */
word ingc;
word ineval;
word gcstart;
word evalstart;
int usegc;

word vm();
void exit(int rval);
void *realloc(void *ptr, size_t size);
void free(void *ptr);

#define make_immediate(value, type)  (((value) << 12) | ((type) << 3) | 2)
#define make_header(size, type)      (((size) << 12) | ((type) << 3) | 6)
#define make_raw_header(size, type)  (((size) << 12) | ((type) << 3) | 2054)
#define headerp(val)                 (((val) & 6) == 6)
#define fixnum(val)                  (((val) << 12) | 2) 
#define negative_fixnum(val)         (((val) << 12) | 258)
#define fixval(desc)                 ((desc) >> 12)
#define fixnump(desc)                (((desc)&4095) == 2)
#define fixnums(a,b)                 fixnump((a)|(b))
#define scale(p,root)                ((word) p + (word) root)
#define MAXOBJ 0xffff
#define fliptag(ptr)                 ((word)ptr^2) /* make a pointer look like some (usually bad) immediate object */

#define NR 190 /* fixme, should be ~32*/
static word *fp;

#define RAWBIT             2048
#define header(x)          *(word *x)
#define imm_type(x)        (((x) >> 3) & 0xff)
#define imm_majortype(x)   (((x) >> 3) & 31)
#define imm_val(x)         ((x) >> 12)
#define immediatep(x)      (((word)x)&2)
#define allocp(x)          (!immediatep(x))
#define rawp(hdr)          ((hdr)&RAWBIT)
#define next(n)            ip += n; break;
#define stringp(ob)        (allocp(ob) && rawp(*ob))
#define pairp(ob)          (allocp(ob) && *((word *)ob)==PAIRHDR)
#define INULL  10
#define IFALSE 18
#define ITRUE 274
#define IHALT  10 /* null, convert to a new immediate later */
#define TEXEC    0
#define TPAIR    1
#define TTUPLE   2
#define TFF      8
#define TINT     9      /* positive (big) integer */
#define TBYTES  11      /* a small byte vector */
#define FFRED  128      /* FF options */
#define FFLEFT  64
#define FFRIGHT 32
#define TPROC   32      /* EXEC options */
#define TCLOS   64
#define TCODE  256
#define TINTN   41      /* negative (big) integer */
#define TRAT    73      /* rational */
#define cont(n)            *((word *) ((word)n&-4))
#define flagged(n)         (n&1)
#define flag(n)            (((word)n)^1)
#define nextptr(n)         ((word *) n+1)
#define A0                  R[*ip]               
#define A1                  R[ip[1]]
#define A2                  R[ip[2]]
#define A3                  R[ip[3]]
#define A4                  R[ip[4]]
#define A5                  R[ip[5]]
#define G(ptr,n)            ((word *)(ptr))[n]
#define flagged_or_raw(hdr)         (hdr&2049)
#define hdrsize(hdr)                imm_val(hdr)
#define TICKS 10000                 /* # of function calls in a thread quantum  */
#define PAIRHDR 12302
#define NUMHDR  12366
#define allocate(size, to) to = fp; fp += size;
#define error(opcode, a, b) R[4] = fixnum(opcode); R[5] = (word) a; R[6] = (word) b; goto invoke_mcp;
#define likely(x)       __builtin_expect((x),1)
#define unlikely(x)     __builtin_expect((x),0)
#define assert(exp,val,code) if(unlikely(!(exp))) {error(code, val, ITRUE);}
#define assert_not(exp,val,code) if(unlikely(exp)) {error(code, val, ITRUE);}
#define OGOTO(f,n); ob = (word *) R[f]; acc = n; goto apply
#define REF1(o,r) R[r] = ((word *) R[1])[o]
#define RET(n)   ob=(word *)R[3]; R[3] = R[n]; acc = 1; goto apply
#define MEMPAD (NR+2)*8
#define MINGEN 128
#define OCLOSE(proctype) { word size = *ip++, tmp; word *ob; allocate(size, ob); tmp = R[*ip++]; tmp = ((word *) tmp)[*ip++]; *ob = make_header(size, proctype); ob[1] = tmp; tmp = 2; while(tmp != size) { ob[tmp++] = R[*ip++]; } R[*ip++] = (word) ob; }
#define CLOSE1(proctype) { word size = *ip++, tmp; word *ob; allocate(size, ob); tmp = R[1]; tmp = ((word *) tmp)[*ip++]; *ob = make_header(size, proctype); ob[1] = tmp; tmp = 2; while(tmp != size) { ob[tmp++] = R[*ip++]; } R[*ip++] = (word) ob; }

static __inline__ void rev(word pos) {
   word val = *((word *) pos);
   word next = cont(val);
   *(word *) pos = next;
   cont(val) = (val&1)^(pos|1);
}

static __inline__ word *chase(word *pos) {
   word val = cont(pos);
   if (headerp(val))
      return(pos);
   while (allocp(val) && flagged(val)) {
      pos = (word *) val;
      val = cont(pos);
   }
   return(pos);
}

static void marks(word *pos, word *end) {
   while (pos != end) {
      word val = *pos;
      if (allocp(val) && val >= ((word) genstart)) { 
         if (flagged(val)) {
            pos = ((word *) flag(chase((word *) val))) - 1;
         } else {
            word hdr = *((word *) val);
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
            if (headerp(*new) && flagged(*new)) {
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
   return(new); 
}

/* the GC is a generational variant of the threading collector described in 
   "Efficient Garbage Compaction Algorithm"-paper by Johannes Martin (1982) */

void fix_pointers(word *pos, int delta, word *end) {
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
int adjust_heap(int cells) {
   /* add new realloc + heap fixer here later */
   word *old = memstart;
   word nwords = memend - memstart + MEMPAD; /* MEMPAD is after memend */
   word new_words = nwords + cells;
   if (!usegc) /* only run when the vm is running (temp) */
      return(0);
   memstart = realloc(memstart, new_words*W);
   if (memstart == old) { /* whee, no heap slide \o/ */
      memend = memstart + new_words - MEMPAD; /* leave MEMPAD words alone */
      return(0);
   } else if (memstart) { /* d'oh! we need to O(n) all the pointers... */
      int delta = (word)memstart - (word)old;
      memend = memstart + new_words - MEMPAD; /* leave MEMPAD words alone */
      fix_pointers(memstart, delta, memend); /* todo: measure time spent here */
      return(delta);
   } else {
      puts("realloc failed, out of memory?");
      exit(3);
   }
}

/* input desired allocation size and root object, 
   return a pointer to the same object after compaction, resizing, possible heap relocation etc */
static word *gc(int size, word *regs) {
   word *root;
   word *realend = memend;
   int nfree, nused;
   word now = clock();
   ineval += now - evalstart;
   evalstart = now;
   fp = regs + imm_val(*regs);
   root = fp+1;
   *root = (word) regs;
   fflush(stdout);
   memend = fp;
   marks(root, fp);
   fp = compact();
   now = clock();
   ingc += now - evalstart;
   evalstart = now;
   fflush(stdout);
   regs = (word *) *root;
   memend = realend;
   nfree = (word)memend - (word)regs;
   nused = (word)regs - (word)genstart; 
   if (genstart == memstart) {
      word heapsize = (word) memend - (word) memstart;
      word nused = heapsize - nfree;
      /* printf("GC: %dms in eval, %dms in gc, %d%% in gc\n", ineval/(CLOCKS_PER_SEC/1000), ingc/(CLOCKS_PER_SEC/1000), ingc/((ineval+ingc)/100)); */
      if ((heapsize/(1024*1024)) > max_heap_mb) { 
         breaked |= 8; /* will be passed over to mcp at thread switch*/
      }
      nfree -= size*W + MEMPAD;   /* how much really could be snipped off */
      if (nfree < (heapsize / 20) || nfree < 0) {
         /* increase heap size if less than 5% is free by ~10% of heap size (growth usually implies more growth) */
         regs[imm_val(*regs)] = 0; /* use an invalid descriptor to denote end live heap data  */
         regs = (word *) ((word)regs + adjust_heap(size*W + nused/10 + 4096));
         nfree = memend - regs;
         if (nfree <= size) {
            /* todo, call a rip cord thunk set by MCP if applicable */
            puts("ovm: could not allocate more space");
            exit(0);
         }
      } else if (nfree > (heapsize/10)) {
         /* decrease heap size if more than 10% is free by 10% of the free space */
         int dec = -(nfree/10);
         int new = nfree - dec;
         if (new > size*W*2 + MEMPAD) {
            regs[imm_val(*regs)] = 0; /* as above */
            regs = (word *) ((word)regs + adjust_heap(dec+MEMPAD*W));
            heapsize = (word) memend - (word) memstart; 
            nfree = (word) memend - (word) regs; 
         } 
      }  
      genstart = regs; /* always start new generation */
   } else if (nfree < MINGEN || nfree < size*W*2) {
         genstart = memstart; /* start full generation */
         return(gc(size, regs));
   } else if ((nfree*100)/(nused+nfree) > 90) { 
      /* (can continue using these kind of generations to
      trade some memory for time (the parent generation 
      may keep freeable memory (for the OS, not owl) a bit
      longer as the major GC intervals become sparser) */
   } else {
      genstart = regs; /* start new generation */
   }
   return(regs);
}

void signal_handler(int signal) {
#ifndef WIN32
   switch(signal) {
      case SIGINT: breaked |= 2; break;
      case SIGPIPE: break;
      default: breaked |= 4;
   }
#endif
}

word get_nat() {
   word result = 0;
   int new, i;
   again:
   i = *hp++;
   new = result << 7;
   if (result != (new >> 7)) exit(9); /* overflow kills */
   result = new + (i & 127);
   if (i & 128) goto again;
   return(result);
}  

word *get_field(word *ptrs, int pos) {
   if (0 == *hp) {
      unsigned char type;
      int value;
      hp++;
      type = *hp++;
      value = get_nat();
      *fp++ = make_immediate(value, type);
   } else {
      word diff = get_nat();
      if (ptrs != NULL) *fp++ = ptrs[pos-diff];
   }
   return(fp);
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
         *fp++ = make_raw_header(size, type + (pads<<5));
         wp = (unsigned char *) fp;
         while (bytes--) { *wp++ = *hp++; };
         while (pads--) { *wp++ = 0; };
         fp = (word *) wp;
         break; }
      default: puts("bad object in heap"); exit(42);
   }
   return(fp);
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
   return(n);
}

void add_signal_handler() {
#ifndef WIN32
   struct sigaction sa;
   sa.sa_handler = signal_handler;
   sigemptyset(&sa.sa_mask);
   sa.sa_flags = SA_RESTART;
   sigaction(SIGINT, &sa, NULL);
   sigaction(SIGPIPE, &sa, NULL);
#endif
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
   return(hp);
}

word boot(int nargs, char **argv) {
   int this, pos, nobjs;
   unsigned char *file_heap = NULL;
   word *entry;
   word *oargs = (word *) INULL;
   word *ptrs;
   word nwords;
   usegc = seccompp = evalstart = 0;
   if (heap == NULL) { /* if no preloaded heap, try to load it from first arg */
      if (nargs < 2) exit(1);
      file_heap = load_heap(argv[1]);
      nargs--; argv++; /* skip vm */
   } else {
      hp = (unsigned char *) &heap;
   }
   max_heap_mb = (W == 4) ? 4096 : 65535; /* can be set at runtime */
   memstart = genstart = fp = (word *) realloc(NULL, (0xffff + MEMPAD)*W); /* at least one argument string always fits */
   if (!memstart) exit(3);
   memend = memstart + 0xffff - MEMPAD;
   this = nargs-1;
   usegc = 1;
   while(this >= 0) { /* build an owl string list to oargs at bottom of heap */
      char *str = argv[this];
      char *pos = str;
      int pads;
      word *tmp;
      int len = 0, size;
      while(*pos++) len++;
      if (len > 0xffff) {
         puts("owl: command line argument too long");
         exit(1);
      }
      size = ((len % W) == 0) ? (len/W)+1 : (len/W) + 2;
      if ((word)fp + size >= (word)memend) {
         oargs = gc(0xffff, oargs); /* oargs points to topmost pair, may move as a result of gc */
         fp = oargs + 3;
      }
      pads = (size-1)*W - len;
      tmp = fp;
      fp += size;
      *tmp = make_raw_header(size, 3 + (pads<<5));
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
   add_signal_handler();
   /* can i has nonblocking stdio */
   set_nonblock(0);
   set_nonblock(1);
   set_nonblock(2);
   /* clear the pointers */
   /* fixme, wrong when heap has > 65534 objects */
   ptrs[0] = make_raw_header(nobjs+1,0);
   if (file_heap != NULL) free((void *) file_heap);
   return(vm(entry, oargs));
}

static void bytecopy(char *from, char *to, int n) { while(n--) *to++ = *from++; }
static void wordcopy(word *from, word *to, int n) { while(n--) *to++ = *from++; }

static word prim_connect(word *host, word port) {
   int sock;
   int n;
   struct sockaddr_in addr;
   port = fixval(port);
   if (!allocp(host))  /* bad host type */
      return(IFALSE);
   n = (hdrsize(*host) - 1) * W;
   if ((sock = socket(PF_INET, SOCK_STREAM, 0)) == -1)
      return(IFALSE);
   addr.sin_family = AF_INET;
   addr.sin_port = htons(port);
   addr.sin_addr.s_addr = (in_addr_t) host[1];
   bytecopy((char *) (host + 1), (char *) &addr.sin_addr.s_addr, n);
   if (connect(sock, (struct sockaddr *) &addr, sizeof(struct sockaddr_in)) < 0)
      return(IFALSE);
   set_nonblock(sock);
   return(fixnum(sock));
}

static word prim_less(word a, word b) {
   if (immediatep(a)) {
      if (immediatep(b)) {
         return((a < b) ? ITRUE : IFALSE);
      }
      return(ITRUE); /* imm < alloc */
   }
   if (immediatep(b)) {
      return(IFALSE); /* alloc > imm */
   }
   return((a < b) ? ITRUE : IFALSE);
}

static word prim_cast(word *ob, int type) {
   if (immediatep((word)ob)) {
      return(make_immediate(imm_val((word)ob), type));
   } else { /* make a clone of more desired type */
      word hdr = *ob++;
      int size = hdrsize(hdr);
      word *new, *res; /* <- could also write directly using *fp++ */
      allocate(size, new);
      res = new;
      /* #b11111111111111111111100000000111 */
      *new++ = (hdr&(~2040))|(type<<3);
      wordcopy(ob,new,size-1);
      return((word)res);
   }
}

static int prim_refb(word pword, int pos) {
   word *ob = (word *) pword;
   word hdr, hsize;
   if (immediatep(ob))
      return(-1);
   hdr = *ob;
   hsize = ((imm_val(hdr)-1)*W) - ((hdr>>8)&7); /* bytes - pads */ 
   if (pos >= hsize) 
      return(IFALSE);
   return(fixnum(((unsigned char *) ob)[pos+W]));
}

static word prim_ref(word pword, word pos)  {
   word *ob = (word *) pword;
   word hdr, size;
   pos = fixval(pos);
   if(immediatep(ob)) { return(IFALSE); }
   hdr = *ob;
   if (rawp(hdr)) { /* raw data is #[hdrbyte{W} b0 .. bn 0{0,W-1}] */ 
      size = ((imm_val(hdr)-1)*W) - ((hdr>>8)&7);
      if (pos >= size) { return(IFALSE); }
      return(fixnum(((unsigned char *) ob)[pos+W]));
   }
   size = imm_val(hdr);
   if (!pos || size <= pos) /* tuples are indexed from 1 (probably later 0-255)*/
      return(IFALSE);
   return(ob[pos]);
}

static word prim_set(word wptr, word pos, word val) {
   word *ob = (word *) wptr;
   word hdr;
   word *new;
   int p = 0;
   pos = fixval(pos);
   if(immediatep(ob)) { return(IFALSE); }
   hdr = *ob;
   if (rawp(hdr) || imm_val(hdr) < pos) { return(IFALSE); }
   hdr = imm_val(hdr); /* get size */
   allocate(hdr, new);
   while(p <= hdr) {
      new[p] = (pos == p && p) ? val : ob[p];
      p++;
   }
   return((word) new);
}

/* make a byte vector object to hold len bytes (compute size, advance fp, set padding count */
static word *mkbvec(int len, int type) {
   int nwords = (len/W) + ((len % W) ? 2 : 1);
   int pads = (nwords-1)*W - len;
   word *ob = fp;
   fp += nwords;
   *ob = make_raw_header(nwords, type|(pads<<5));
   return(ob);
}

/* system- and io primops */
static word prim_sys(int op, word a, word b, word c) {
   switch(op) {
      case 0: { /* 0 fsend fd buff len r → n if wrote n, 0 if busy, False if error (argument or write) */
         int fd = fixval(a);
         word *buff = (word *) b;
         int wrote, size, len = fixval(c);
         if (immediatep(buff)) return(IFALSE);
         size = (imm_val(*buff)-1)*W;
         if (len > size) return(IFALSE);
         wrote = write(fd, ((char *)buff)+W, len);
         if (wrote > 0) return(fixnum(wrote));
         if (errno == EAGAIN) return(fixnum(0));
         return(IFALSE); }
      case 1: { /* 1 = fopen <str> <mode> <to> */
         char *path = (char *) a;
         int mode = fixval(b);
         int val;
         struct stat sb;
         if (!(allocp(path) && imm_type(*path) == 3))
            return(IFALSE);
         mode |= O_BINARY | ((mode > 0) ? O_CREAT | O_TRUNC : 0);
         val = open(((char *) path) + W, mode,(S_IRUSR|S_IWUSR));
         if (val < 0 || fstat(val, &sb) == -1 || sb.st_mode & S_IFDIR) {
            close(val);
            return(IFALSE);
         }
         set_nonblock(val);
         return(fixnum(val)); }
      case 2: 
         return(close(fixval(a)) ? IFALSE : ITRUE);
      case 3: { /* 3 = sopen port -> False | fd  */
         int port = fixval(a);
         int s;
         int opt = 1; /* TRUE */
         struct sockaddr_in myaddr;
         myaddr.sin_family = AF_INET;
         myaddr.sin_port = htons(port);
         myaddr.sin_addr.s_addr = INADDR_ANY;
         s = socket(AF_INET, SOCK_STREAM, 0);
         if (s < 0) return(IFALSE);
         if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&opt, sizeof(opt)) \
             || bind(s, (struct sockaddr *) &myaddr, sizeof(myaddr)) != 0 \
             || listen(s, 5) != 0) {
            close(s);
            return(IFALSE);
         }
         set_nonblock(s);
         return(fixnum(s)); }
      case 4: { /* 4 = accept port -> rval=False|(ip . fd) */
         int sock = fixval(a);
         struct sockaddr_in addr;
         socklen_t len = sizeof(addr);
         int fd;
         word *pair;
         char *ipa;
         fd = accept(sock, (struct sockaddr *)&addr, &len);
         if (fd < 0) return(IFALSE); /*   A2 = (errno == EWOULDBLOCK || errno == EAGAIN) ? ITRUE : IFALSE; */
         set_nonblock(fd);
         ipa = (char *) &addr.sin_addr;
         *fp = make_raw_header(2, TBYTES|((4%W)<<5));
         bytecopy(ipa, ((char *) fp) + W, 4);
         fp[2] = PAIRHDR;
         fp[3] = (word) fp;
         fp[4] = fixnum(fd);
         pair = fp+2;
         fp += 5;
         return((word) pair); }
      case 5: { /* fread fd max -> obj | eof | F (read error) | T (would block) */
         word fd = fixval(a);
         word max = fixval(b); 
         word *res;
         int n, nwords = (max/W) + 2;
         allocate(nwords, res);
#ifndef WIN32
         n = read(fd, ((char *) res) + W, max);
#else
         if (fd == 0) { /* ... */
            if(_kbhit()) {
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
            *res = make_raw_header(read_nwords, TBYTES|(pads<<5));
            return((word)res);
         }
         fp = res;
         if (n == 0) { /* EOF */
            return(make_immediate(0, 4)); 
         }
         return((errno == EAGAIN || errno == EWOULDBLOCK) ? ITRUE : IFALSE); }
      case 6:
         exit(fixval(a)); /* stop the press */
      case 7: /* set memory limit (in mb) */
         max_heap_mb = fixval(a);
         return(a);
      case 8: /* get machine word size (in bytes) */
         return(fixnum(W));
      case 9: /* get memory limit (in mb) */
         return(fixnum(max_heap_mb));
#ifdef USE_SECCOMP 
      case 10: /* enter linux seccomp mode */
         if (prctl(PR_SET_SECCOMP,1)) {
            return(IFALSE);
         }
         seccompp = 1;
         return(ITRUE);
#endif
      /* dirops only to be used via exposed functions */
      case 11: { /* sys-opendir path _ _ -> False | dirobjptr */
         char *path = W + (char *) a; /* skip header */
         DIR *dirp = opendir(path);
         if(!dirp) return(IFALSE);
         return(fliptag(dirp)); }
      case 12: { /* sys-readdir dirp _ _ -> bvec | eof | False */
         DIR *dirp = (DIR *)fliptag(a);
         word *res;
         unsigned int len;
         struct dirent *dire = readdir(dirp);
         if (!dire) return(make_immediate(0, 4)); /* eof at end of dir stream */
         len = strlen(dire->d_name);
         if (len > 0xffff) return(IFALSE); /* false for errors, like too long file names */
         res = mkbvec(len, 3); /* make a fake raw string (OS may not use valid UTF-8) */
         bytecopy((char *)&dire->d_name, (char *) (res + 1), len); /* *no* terminating null, this is an owl bvec */
         return((word)res); }
      case 13: /* sys-closedir dirp _ _ -> ITRUE */
         closedir((DIR *)fliptag(a));
         return(ITRUE);
      default: 
         return(IFALSE);
   }
}

static word prim_lraw(word wptr, int type, word revp) {
   word *lst = (word *) wptr;
   int nwords, len = 0, step, pads;
   unsigned char *pos;
   word *raw, *ob;
   if (revp != IFALSE) { exit(1); } /* <- to be removed */
   ob = lst;
   while (allocp(ob) && *ob == PAIRHDR) {
      len++;
      ob = (word *) ob[2];
   }
   if ((word) ob != INULL) return(IFALSE);
   if (len > 0xffff) return(IFALSE);
   nwords = (len/W) + ((len % W) ? 2 : 1);
   allocate(nwords, raw);
   pads = (nwords-1)*W - len; /* padding byte count, usually stored to top 3 bits */
   *raw = make_raw_header(nwords, (type|(pads<<5)));
   ob = lst;
   pos = ((unsigned char *) raw) + W;
   step = 1;
   while ((word) ob != INULL) {
      *pos++ = fixval(ob[1])&255;
      ob = (word *) ob[2];
   }
   while(pads--) { *pos++ = 0; } /* clear the padding bytes */
   return((word)raw);
}


static word prim_mkff(word t, word l, word k, word v, word r) {
   word *ob = fp;
   ob[1] = k;
   ob[2] = v;
   if (l == IFALSE) {
      if (r == IFALSE) {
         *ob = make_header(3, t); 
         fp += 3;
      } else {
         *ob = make_header(4, t|FFRIGHT); 
         ob[3] = r;
         fp += 4;
      }
   } else if (r == IFALSE) { 
      *ob = make_header(4, t|FFLEFT); 
      ob[3] = l;
      fp += 4;
   } else {
      *ob = make_header(5, t|FFLEFT|FFRIGHT);
      ob[3] = l;
      ob[4] = r;
      fp += 5;
   }
   return((word) ob);
}

word vm(word *ob, word *args) {
   unsigned char *ip;
   int bank = 0; /* ticks deposited at syscall */
   int ticker = TICKS; /* any initial value ok */
   unsigned short acc = 0; /* no support for >255arg functions */
   int op; /* opcode to execute */
   struct timeval tv;
   static word R[NR];
   word load_imms[] = {fixnum(0), INULL, ITRUE, IFALSE};  /* for ldi and jv */
   usegc = 1; /* enble gc (later have if always evabled) */
   tv.tv_sec = 0;
   tv.tv_usec = 20000;
   evalstart = clock();

   /* clear blank regs */
   while(acc < NR) { R[acc++] = INULL; }
   R[0] = IFALSE;
   R[3] = IHALT;
   R[4] = (word) args;
   acc = 2; /* boot always calls with 2 args*/

apply: /* apply something at ob to values in regs, or maybe switch context */
   if (likely(allocp(ob))) {
      word hdr = *ob & 4095;
      if (hdr == 262) { /* proc  */ 
         R[1] = (word) ob; ob = (word *) ob[1];
      } else if (hdr == 518) { /* clos */
         R[1] = (word) ob; ob = (word *) ob[1];
         R[2] = (word) ob; ob = (word *) ob[1];
      } else if ((hdr & 2303) != 2054) { /* not even code */
         error(259, ob, INULL);
      } 
      if (unlikely(!ticker--)) goto switch_thread;
      ip = ((unsigned char *) ob) + W;
      if (likely(*ip++ == acc)) goto invoke;
      ip--;
      error(256, fixnum(*ip), ob);
   } else if ((word)ob == IHALT) {
      /* a tread or mcp is calling the final continuation  */
      ob = (word *) R[0];
      if (allocp(ob)) {
         R[0] = IFALSE;
         breaked = 0;
         R[4] = R[3];
         R[3] = fixnum(2);
         R[5] = IFALSE;
         R[6] = IFALSE;
         ticker = 0xffffff;
         bank = 0;
         acc = 4;
         goto apply;
      }
      return(fixval(R[3]));
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
      *state = make_header(acc, TTUPLE);
      state[acc-1] = R[acc];
      while(pos < acc-1) {
         state[pos] = R[pos];
         pos++;
      }
      ob = (word *) R[0]; 
      R[0] = IFALSE; /* remove mcp cont */
      /* R3 marks the syscall to perform */
      R[3] = breaked ? ((breaked & 8) ? fixnum(14) : fixnum(10)) : fixnum(1);
      R[4] = (word) state;
      R[5] = IFALSE;
      R[6] = IFALSE;
      acc = 4;
      breaked = 0;
      goto apply;
   }
invoke: /* nargs and regs ready, maybe gc and execute ob */
   if (((word)fp) + 1024*64 >= ((word) memend)) { /* <- fixme... can be lowered after the compiler pass */
      int p = 0; 
      *fp = make_header(NR+1, 50); 
      R[NR-1] = (word) ob; /* fixme: unsafe temp location */
      while(p < NR) { fp[p+1] = R[p]; p++; } 
      fp = gc(1024*64, fp);
      evalstart = clock();
      while(--p >= 0) { R[p] = fp[p+1]; }
      ob = (word *) R[NR-1];
      ip = ((unsigned char *) ob) + W + 1;
   }
   op = *ip++;
   if (op) {
      goto dispatch;
   } else {
      op = *ip<<8 | ip[1];
      goto super_dispatch;
   }
   
next_op:
   op = *ip++;
dispatch: /* handle normal bytecode */
   switch(op&63) {
      case 0: op = (*ip << 8) | ip[1]; goto super_dispatch;
      case 1: {word *ob = (word *)R[*ip]; R[ip[2]] = ob[ip[1]]; next(3);}
      case 2: OGOTO(*ip,ip[1]); /* fixme, these macros are not used in cgen output anymore*/
      case 3: OCLOSE(TCLOS); break;
      case 4: OCLOSE(TPROC); break;
      case 5: REF1(*ip,ip[1]); next(2);
      case 6: CLOSE1(TCLOS); break;
      case 7: CLOSE1(TPROC); break;
      case 8: /* jlq a b o, extended jump  */
         if(R[*ip] == A1) { ip += ip[2] + (ip[3] << 8); } 
         next(4); 
      case 9: R[ip[1]] = R[*ip]; next(2);
      case 10: { /* type o r */
         word ob = R[*ip++];
         if (allocp(ob)) ob = *((word *) ob);
         R[*ip++] = fixnum(ob&4095);
         break; }
      case 11: { /* jit2 a t ol oh */
         word a = R[*ip];
         if (immediatep(a) && ((a>>3)&0xff) == ip[1]) {
            ip += ip[2] + (ip[3] << 8);
         }
         next(4); }
      case 12: { /* jat2 a t ol oh */
         word a = R[*ip];
         if (allocp(a) && (((*((word *) a))>>3)&0x1ff) == ip[1]) {
            ip += ip[2] + (ip[3] << 8);
         }
         next(4); }
      case 13: /* ldi{2bit what} [to] */
         R[*ip++] = load_imms[op>>6];
         break;
      case 14: R[ip[1]] = fixnum(*ip); next(2);
#ifdef NATIVECALL
      case 15: { /* testing */
         A4 = ((word (*)(word, word, word))(A0+W))(A1, A2, A3); 
         next(5); }
#endif
      case 16: /* jv[which] a o1 a2*/
         if(R[*ip] == load_imms[op>>6]) { ip += ip[1] + (ip[2] << 8); } 
         next(3); 
      case 18: /* goto-code p */
         ob = (word *) R[*ip]; /* needed in case of gc */
         ip = ((unsigned char *) R[*ip]) + W + 1;
         goto invoke;
      case 19: { /* goto-proc p */
         word *this = (word *) R[*ip];
         R[1] = (word) this;
         ob = (word *) this[1];
         ip = ((unsigned char *) ob) + W + 1;
         goto invoke; }
      case 21: { /* goto-clos p */
         word *this = (word *) R[*ip];
         R[1] = (word) this;
         this = (word *) this[1];
         R[2] = (word) this;
         ob = (word *) this[1];
         ip = ((unsigned char *) ob) + W + 1;
         goto invoke;
         break; }
      case 22: { /* cast o t r */
         word *ob = (word *) R[*ip];
         word type = fixval(A1) & 0xff;
         A2 = prim_cast(ob, type);
         next(3); }
      case 23: { /* mkt t s f1 .. fs r */
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
         next(s+1); }
      case 24: /* ret val */
         ob = (word *) R[3];
         R[3] = R[*ip];
         acc = 1;
         goto apply; 
      case 26: { /* fxqr ah al b qh ql r, b != 0, int32 / int16 -> int32, as fixnums */
         word a = (fixval(A0)<<16) | fixval(A1); 
         word b = fixval(A2);
         word q = a / b;
         A3 = fixnum(q>>16);
         A4 = fixnum(q&0xffff);
         A5 = fixnum(a - q*b);
         next(6); }
      case 27: /* syscall cont op arg1 arg2 */
         ob = (word *) R[0];
         R[0] = IFALSE;
         R[3] = A1; R[4] = R[*ip]; R[5] = A2; R[6] = A3;
         acc = 4;
         if (ticker > 10) bank = ticker; /* deposit remaining ticks for return to thread */
         goto apply;
      case 28: { /* sizeb obj to */ /* todo: to be merged with size? */
         word ob = R[*ip];
         if (immediatep(ob)) {
            A1 = fixnum(0);
         } else {
            word hdr = *((word *) ob);
            A1 = (rawp(hdr)) ? fixnum((hdrsize(hdr)-1)*W - ((hdr >> 8) & 7)) : fixnum(0);
         }
         next(2); }
      case 29: { /* ncons a b r */
         *fp = NUMHDR;
         fp[1] = A0;
         fp[2] = A1;
         A2 = (word) fp;
         fp += 3;
         next(3); }
      case 30: { /* ncar a rd */
         word *ob = (word *) R[*ip];
         assert(allocp(ob), ob, 30);
         A1 = ob[1];
         next(2); }
      case 31: { /* ncdr a r */
         word *ob = (word *) R[*ip];
         assert(allocp(ob), ob, 31);
         A1 = ob[2];
         next(2); }
      case 32: { /* bind tuple <n> <r0> .. <rn> */
         word *tuple = (word *) R[*ip++];
         word hdr, pos = 1, n = *ip++;
         assert(allocp(tuple), tuple, 32);
         hdr = *tuple;
         assert_not((rawp(hdr) || fixval(hdr)-1 != n), tuple, 32);
         while(n--) { R[*ip++] = tuple[pos++]; }
         break; }
      case 33: { /* jrt a t o, jump by raw type (ignoring padding info) */
         word a = R[*ip];
         if (allocp(a) && (*(word *)a & 2296) == (2048 | (ip[1]<<3))) {
				ip += ip[2];
         }
         next(3); }
      case 34: { /* connect <host-ip> <port> <res> -> fd | False, via an ipv4 tcp stream */
         A2 = prim_connect((word *) A0, A1); /* fixme: remove and put to prim-sys*/
         next(3); }
      case 35: { /* listuple type size lst to */
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
         next(4); }
      case 36: { /* size o r */
         word ob = R[*ip++];
         R[*ip++] = (immediatep(ob)) ? fixnum(0) : fixnum(imm_val(V(ob))-1);
         break; }
      case 37: { /* ms r */
#ifndef WIN32
         if (!seccompp)
            usleep(fixval(A0)*1000);
#else
         Sleep(fixval(A0));
#endif
         A1 = (errno == EINTR) ? ITRUE : IFALSE; 
         next(2); }
      case 38: { /* fx+ a b r o, types prechecked, signs ignored */
         word res = fixval(R[*ip]) + fixval(A1);
         if (res & 0xffff0000) {
            A2 = fixnum(res&0xffff);
            A3 = ITRUE;
         } else {
            A2 = fixnum(res);
            A3 = IFALSE;
         }
         next(4); }
      case 39: { /* fx* a b l h */
         word res = fixval(R[*ip]) * fixval(A1);
         A2 = fixnum(res&0xffff);
         A3 = fixnum((res>>16)&0xffff);
         next(4); }
      case 40: { /* fx- a b r u, args prechecked, signs ignored */
         word r = (fixval(A0)|0x10000) - fixval(A1);
         A3 = (r & 0x10000) ? IFALSE : ITRUE;
         A2 = fixnum(r&0xffff);
         next(4); }
      case 41: { /* red? node r (has highest type bit?) */
         word *node = (word *) R[*ip];
         A1 = (allocp(node) && ((*node)&(FFRED<<3))) ? ITRUE : IFALSE;
         next(2); }
      case 42: /* mkblack l k v r t */ 
         A4 = prim_mkff(TFF,A0,A1,A2,A3);
         next(5);
      case 43: /* mkred l k v r t */ 
         A4 = prim_mkff(TFF|FFRED,A0,A1,A2,A3);
         next(5);
      case 44: /* less a b r */
         A2 = prim_less(A0, A1);
         next(3);
      case 45: { /* set t o v r */
         A3 = prim_set(A0, A1, A2);
         next(4);
         break; }
      case 46: { /* fftoggle node to (probably useless to have as a separate primop, as are some other ff things ) */
         word *node = (word *) R[*ip];
         word *new, h;
         assert(allocp(node), node, 46);
         new = fp;
         h = *node++;
         A1 = (word) new;
         *new++ = (h^(FFRED<<3));
         switch(hdrsize(h)) {
            case 5:  *new++ = *node++;
            case 4:  *new++ = *node++;
            default: *new++ = *node++;
                     *new++ = *node++; }
         fp = new;
         next(2); }
      case 47:  /* ref t o r */ /* fixme: deprecate this later */
         A2 = prim_ref(A0, A1);
         next(3);
         break;
      case 48: { /* refb t o r */ /* todo: merge with ref, though 0-based  */
         A2 = prim_refb(A0, fixval(A1));
         next(3); }
      case 49: { /* withff node l k v r */
         word hdr, *ob = (word *) R[*ip];
         assert(allocp(ob), ob, 49);
         hdr = *ob >> 3;
         assert(((hdr&31)==TFF),ob,49) 
         A2 = ob[1]; /* key */
         A3 = ob[2]; /* value */
         if (hdr&FFLEFT) {         
            A1 = ob[3];
            A4 = (hdr&FFRIGHT) ? ob[4] : IFALSE; 
         } else {
            A1 = IFALSE;
            A4 = (hdr&FFRIGHT)?ob[3]:IFALSE;
         }
         next(5); }
      case 50: { /* run thunk quantum */ /* fixme: maybe move to sys */
         word hdr;
         ob = (word *) R[*ip];
         R[0] = R[3];
         ticker = bank ? bank : TICKS;
         bank = 0;
         assert(allocp(ob),ob,50);
         hdr = *ob;
         if (imm_type(hdr) == TTUPLE) {
            int pos = hdrsize(hdr) - 1;
            word code = ob[pos];
            acc = pos - 3;
            while(--pos) { R[pos] = ob[pos]; }
            ip = ((unsigned char *) code) + W + 1;
         } else {
            /* call a thunk with terminal continuation */
            R[3] = IHALT; /* exit via R0 when the time comes */
            acc = 1;
            goto apply;
         }
         break; }
      case 51: { /* cons a b r */
         *fp = PAIRHDR;
         fp[1] = A0;
         fp[2] = A1;
         A2 = (word) fp;
         fp += 3;
         next(3); }
      case 52: { /* car a r */
         word *ob = (word *) R[*ip++];
         assert(pairp(ob), ob, 52);
         R[*ip++] = ob[1];
         break; }
      case 53: { /* cdr a r */
         word *ob = (word *) R[*ip++];
         assert(pairp(ob), ob, 52);
         R[*ip++] = ob[2];
         break; }
      case 54: /* eq a b r */
         A2 = (R[*ip] == A1) ? ITRUE : IFALSE;
         next(3);
      case 55: { /* band a b r, prechecked */
         word a = R[*ip];
         word b = A1;
         A2 = a & b;
         next(3); }
      case 56: { /* bor a b r, prechecked */
         word a = R[*ip];
         word b = A1;
         A2 = a | b;
         next(3); }
      case 57: { /* bxor a b r, prechecked */
         word a = R[*ip];
         word b = A1;
         A2 = a ^ (b ^ 2); /* clear fixnum tag from b */
         next(3); }
      case 58: { /* fx>> a b hi lo */
         word r = fixval(A0) << (16 - fixval(A1));
         A2 = fixnum(r>>16);
         A3 = fixnum(r&0xffff);
         next(4); }
      case 59: { /* fx<< a b hi lo */
         word res = fixval(R[*ip]) << fixval(A1);
         A2 = fixnum(res>>16);
         A3 = fixnum(res&0xffff);
         next(4); }
      case 60: /* lraw lst type dir r (fixme, alloc amount testing compiler pass not in place yet!) */
         A3 = prim_lraw(A0, fixval(A1), A2);
         next(4);
      case 61: /* clock <secs> <ticks> */ { /* fixme: sys */
         struct timeval tp;
         word *ob;
         gettimeofday(&tp, NULL);
         allocate(6, ob); /* [NUM hi [NUM lo null]] */
         ob[0] = ob[3] = NUMHDR; /* make a bignum for posix time */
         ob[1] = fixnum(tp.tv_sec >> 16);
         ob[4] = fixnum(tp.tv_sec&0xffff);
         ob[2] = INULL; 
         ob[5] = (word) ob;
         A0 = (word) (ob + 3);
         A1 = fixnum(tp.tv_usec / 1000);
         next(2); }
      case 62: /* set-ticker <val> <to> -> old ticker value */ /* fixme: sys */
         A1 = fixnum(ticker&0xffff);
         ticker = fixval(A0);
         next(2); 
      case 63: { /* sys-prim op arg1 arg2 arg3 r1 */
         A4 = prim_sys(fixval(A0), A1, A2, A3);
         next(5); }
      default: /* bad instruction */
         ip--;
         error(258, fixnum(*ip), INULL);
   }
   goto next_op;

super_dispatch: /* run macro instructions */
   switch(op) {
/* AUTOGENERATED INSTRUCTIONS */
      default:
         error(258, fixnum(op), ITRUE);
   }
   goto apply;

invoke_mcp: /* R4-R6 set, set R3=cont and R4=syscall and call mcp */
   ob = (word *) R[0];
   R[0] = IFALSE;
   R[3] = fixnum(3);
   if (allocp(ob)) {
      acc = 4;
      goto apply;
   }
   /* acc = write(2,"owl: Daisy, Daisy...", 20); */
   return(1);
}

int main(int nargs, char **argv) {
   return(boot(nargs, argv));
}
