package tarski;

import com.intellij.util.SmartList;
import scala.collection.immutable.$colon$colon$;
import scala.collection.immutable.List;
import scala.collection.immutable.Nil$;
import tarski.Items.*;
import tarski.Scores.Alt;
import tarski.Scores.Best;
import tarski.Scores.Empty$;
import tarski.Scores.Scored;

import java.util.Arrays;

import static tarski.JavaScores.pp;

public class QueriableItemList implements Tries.Queriable<Item>, ValueByItemQuery {
  private final Item[] items;

  public static final QueriableItemList empty = new QueriableItemList(new Item[0]);

  private QueriableItemList(final Item[] items) {
    this.items = items;
  }

  public Item[] array() {
    return items;
  }

  public QueriableItemList add(final Item item) {
    final Item[] old = this.items;
    final Item[] items = Arrays.copyOf(old,old.length+1);
    items[old.length] = item;
    return new QueriableItemList(items);
  }

  public QueriableItemList add(final Item[] added) {
    final Item[] old = this.items;
    final Item[] items = Arrays.copyOf(old, old.length+added.length);
    System.arraycopy(added, 0, items, old.length, added.length);
    return new QueriableItemList(items);
  }

  @Override public List<Item> exact(final char[] ss) {
    final String s = new String(ss);
    List<Item> results = (List)Nil$.MODULE$;
    for (final Item item : items) {
      if (item.name().equals(s))
        results = $colon$colon$.MODULE$.apply(item,results);
    }
    return results;
  }

  @Override public Scored<Item> typoQuery(final char[] ctyped) {
    final String typed = new String(ctyped);
    List<Alt<Item>> results = (List)Nil$.MODULE$;
    for (final Item item : items) {
      final String meant = item.name();
      if (!meant.equals(typed)) {
        //final JavaScores.DebugProb p = Pr.typoProbability(meant,typed);
        final double p = Pr.typoProbability(meant,typed);
        if (pp(p) > Pr.minimumProbability())
          results = $colon$colon$.MODULE$.apply(new Alt<Item>(p,item),results);
      }
    }
    return JavaScores.listGood(results);
  }

  @Override public Scored<ValueOrMethod> query(final TypeItem t) {
    Scored<Value> vs = (Scored)Empty$.MODULE$;
    Scored<MethodItem> ms = (Scored)Empty$.MODULE$;
    for (final Item i : items) {
      if (i instanceof Value) {
        final Value v = (Value) i;
        if (Types.isSubitem(v.item(),t))
          vs = new Best<Value>(JavaScores.one,v,vs);
      } else if (i instanceof MethodItem) {
        final MethodItem m = (MethodItem) i;
        final TypeItem ret = m.retItem();
        if (JavaItems.considerMethod(m,ret) && Types.isSubitem(ret,t))
          ms = new Best<MethodItem>(Pr.methodByItem(),m,ms);
      }
    }
    return (Scored)ms.$plus$plus(vs);
  }
}
