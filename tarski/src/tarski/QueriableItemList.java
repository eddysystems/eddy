package tarski;

import scala.collection.JavaConversions;
import scala.collection.immutable.List;

import java.util.ArrayList;
import java.util.Arrays;

public class QueriableItemList implements Tries.Queriable<Items.Item> {
  private final Items.Item[] _items;
  private final ArrayList<Items.Item> _results;
  private final ArrayList<Scores.Alt<Items.Item>> _altResults;

  public static final QueriableItemList empty = new QueriableItemList(new Items.Item[0]);

  private QueriableItemList(Items.Item[] items) {
    _items=items;
    _results = new ArrayList<Items.Item>(_items.length);
    _altResults = new ArrayList<Scores.Alt<Items.Item>>(_items.length);
  }

  public Items.Item[] array() {
    return _items;
  }

  public QueriableItemList add(Items.Item item) {
    Items.Item[] items = Arrays.copyOf(_items, _items.length+1);
    items[_items.length] = item;
    return new QueriableItemList(items);
  }

  @Override
  public List<Items.Item> exact(char[] ss) {
    String s = new String(ss);
    _results.clear();
    for (Items.Item item : _items) {
      if (item.name().equals(s))
        _results.add(item);
    }
    return JavaConversions.asScalaBuffer(_results).toList();
  }

  @Override
  public List<Scores.Alt<Items.Item>> typoQuery(char[] ctyped) {
    String typed = new String(ctyped);
    _altResults.clear();
    for (Items.Item item : _items) {
      String meant = item.name();
      if (meant.equals(typed))
        continue;
      double/*Prob*/ p = Pr.typoProbability(meant,typed);
      if (p > Pr.minimumProbability())
        _altResults.add(new Scores.Alt<Items.Item>(p,item));
    }
    return JavaConversions.asScalaBuffer(_altResults).toList();
  }
}
