package com.eddysystems.eddy.engine;

import org.jetbrains.annotations.NotNull;

public class TypeNameItemNamePair {
  @NotNull final public String typename;
  @NotNull final public String itemname;

  TypeNameItemNamePair(String typename, String itemname) {
    this.typename = typename;
    this.itemname = itemname;
  }
}