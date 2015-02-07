public class Custom extends RuntimeException {
  Custom(String s) {
    supe("prefix" + s)<caret>
  }
}
