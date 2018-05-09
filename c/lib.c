/* library mode init */
void init() {
   int nobjs=0, nwords=0;
   hp = (byte *) &heap; /* builtin heap */
   state = IFALSE;
   heap_metrics(&nwords, &nobjs);
   max_heap_mb = (W == 4) ? 4096 : 65535;
   nwords += nobjs + INITCELLS;
   memstart = genstart = fp = (word *) realloc(NULL, (nwords + MEMPAD)*W); 
   if (!memstart) exit(4);
   memend = memstart + nwords - MEMPAD;
   state = (word) load_heap(nobjs);
}

/* bvec → value library call test with preserved state */
word library_call(size_t len, char *ptr) {
   char *pos;
   int pads;
   word *bvec;
   word res;
   word program_state = state;
   int size;
   state = IFALSE;
   if (program_state == IFALSE) {
      printf("no program state - cannot continue");
      exit(1);
   }
   if (len > FMAX) {
      return (char *) NULL;
   }
   size = ((len % W) == 0) ? (len/W)+1 : (len/W) + 2;
   printf("making an argument byte vector with %d bytes in %d words\n", len, size);     
   pads = (size-1)*W - len;
   bvec = fp;
   fp += size;
   printf("libary call from state %d\n", state);
   *bvec = make_raw_header(size, TBVEC, pads);
   pos = ((byte *) bvec) + W;
   while(len--) *pos++ = *ptr++;
   res = vm(program_state, bvec);
   if(fixnump(res)) {
      printf("Returned %d\n", fixval(res));
   } else {
      printf("Returned descriptor %d\n", res);
   }
   return res;
}

int main(int nargs, char **argv) {
   init();
   char foo[] = {1, 2, 3}; //  0
   char bar[] = {4, 5}; // 6
   char baz[] = {6, 7, 8, 9}; // 15
   library_call(3, &foo);
   library_call(2, &bar);
   library_call(4, &baz);
   return 0;
}


