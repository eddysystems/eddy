package com.eddysystems.eddy.engine;

import org.jetbrains.annotations.NotNull;

public class TypeNameItemNamePair {
  @NotNull final public String typename;
  @NotNull final public String itemname;

  public String toString() {
    return typename + ' ' + itemname;
  }

  TypeNameItemNamePair(String typename, String itemname) {
    this.typename = typename;
    this.itemname = itemname;
  }
}
