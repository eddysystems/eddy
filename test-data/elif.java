class Test {
  int f() {
    if (true) { 
      return 1;
    } elif fals<caret> {
      return 2;
    }
    return 3;
  }
}
