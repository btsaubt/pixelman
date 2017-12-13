def int main() { 
  int i;
  i = 0;
 
  while(i == 4) { 
    i = i + 1;
    if(i == 1) { 
      continue; 
    }

    if(i == 3) {
      break;
    }
  }
  return i;
}
