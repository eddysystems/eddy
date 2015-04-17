package tarski;

import tarski.Items.ValueOrMethod;
import tarski.Items.TypeItem;
import tarski.Scores.Scored;

public interface ValueByItemQuery {
  static final boolean nullaryMethods = false;

  // All values (fields, parameters, local variables) of a type derived from the given item
  Scored<ValueOrMethod> query(final TypeItem type);
}
