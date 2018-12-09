static unsigned int ilog2(unsigned int x) {
  int result = 0;
  while (x >>= 1) result++;
  return result;
}
