package tarski;

import scala.collection.immutable.$colon$colon$;
import scala.collection.immutable.List;
import scala.collection.immutable.Nil$;
import tarski.Items.Item;
import tarski.Scores.Alt;
import tarski.Scores.Scored;
import static tarski.JavaScores.pp;

import java.util.ArrayList;
import java.util.Arrays;

public class QueriableItemList implements Tries.Queriable<Item>, ValueByItemQuery {
  private final Item[] items;

  public static final QueriableItemList empty = new QueriableItemList(new Item[0]);

  private QueriableItemList(final Item[] items) {
    this.items = items;
  }

  public Item[] array() {
    return items;
  }

  public QueriableItemList add(Item item) {
    final Item[] old = this.items;
    final Item[] items = Arrays.copyOf(old,old.length+1);
    items[old.length] = item;
    return new QueriableItemList(items);
  }

  public QueriableItemList add(Item[] added) {
    final Item[] old = this.items;
    final Item[] items = Arrays.copyOf(old, old.length+added.length);
    System.arraycopy(added, 0, items, old.length, added.length);
    return new QueriableItemList(items);
  }

  @Override public List<Item> exact(char[] ss) {
    final String s = new String(ss);
    List<Item> results = (List)Nil$.MODULE$;
    for (final Item item : items) {
      if (item.name().equals(s))
        results = $colon$colon$.MODULE$.apply(item,results);
    }
    return results;
  }

  @Override public Scored<Item> typoQuery(char[] ctyped) {
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

  @Override public Items.Value[] query(Items.TypeItem t) {
    java.util.List<Items.Value> results = new ArrayList<Items.Value>();
    for (Items.Item it : items) {
      if (it instanceof Items.Value && Types.isSubitem(((Items.Value)it).item(), t))
        results.add((Items.Value)it);
    }
    return results.toArray(new Items.Value[results.size()]);
  }
}
