package tarski;

public interface ValueByItemQuery {
  // get all values (fields, parameters, local variables) of a type derived from the given item
  public Items.Value[] query(Items.TypeItem type);
}
