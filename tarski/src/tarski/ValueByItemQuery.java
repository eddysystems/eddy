package tarski;

import tarski.Items.Value;
import tarski.Items.TypeItem;
import tarski.Scores.Scored;

public interface ValueByItemQuery {
  // All values (fields, parameters, local variables) of a type derived from the given item
  public Scored<Value> query(final TypeItem type);
}
