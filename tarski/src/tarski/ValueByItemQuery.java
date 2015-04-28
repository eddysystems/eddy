/* ValueByItemQuery: Look up items by their type */

package tarski;

import tarski.Items.ValueOrMethod;
import tarski.Items.TypeItem;
import tarski.Scores.Scored;

public interface ValueByItemQuery {
  // All values (fields, parameters, local variables) of a type derived from the given item
  Scored<ValueOrMethod> query(final TypeItem type);
}
