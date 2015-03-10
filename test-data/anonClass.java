import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

class X {
  void x() {
    List<X> x = new <caret>List<X>() {
      @Override
      public int size() {
        return 0;
      }

      @Override
      public boolean isEmpty() {
        return false;
      }

      @Override
      public boolean contains(Object o) {
        return false;
      }

      @NotNull
      @Override
      public Iterator<X> iterator() {
        return null;
      }

      @NotNull
      @Override
      public Object[] toArray() {
        return new Object[0];
      }

      @NotNull
      @Override
      public <T> T[] toArray(T[] ts) {
        return null;
      }

      @Override
      public boolean add(X x) {
        return false;
      }

      @Override
      public boolean remove(Object o) {
        return false;
      }

      @Override
      public boolean containsAll(Collection<?> collection) {
        return false;
      }

      @Override
      public boolean addAll(Collection<? extends X> collection) {
        return false;
      }

      @Override
      public boolean addAll(int i, Collection<? extends X> collection) {
        return false;
      }

      @Override
      public boolean removeAll(Collection<?> collection) {
        return false;
      }

      @Override
      public boolean retainAll(Collection<?> collection) {
        return false;
      }

      @Override
      public void clear() {

      }

      @Override
      public X get(int i) {
        return null;
      }

      @Override
      public X set(int i, X x) {
        return null;
      }

      @Override
      public void add(int i, X x) {

      }

      @Override
      public X remove(int i) {
        return null;
      }

      @Override
      public int indexOf(Object o) {
        return 0;
      }

      @Override
      public int lastIndexOf(Object o) {
        return 0;
      }

      @NotNull
      @Override
      public ListIterator<X> listIterator() {
        return null;
      }

      @NotNull
      @Override
      public ListIterator<X> listIterator(int i) {
        return null;
      }

      @NotNull
      @Override
      public List<X> subList(int i, int i1) {
        return null;
      }
    }
  }
}