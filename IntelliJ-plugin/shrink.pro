-injars eddy.zip(!META-INF/**)
-outjars eddy.jar 
-libraryjars /System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Classes/classes.jar

### Keep a few things

-keep class com.eddysystems.eddy.EddyPlugin { 
  <init>(...);
  String getComponentName();
  void initComponent();
  void disposeComponent(); 
  void projectOpened();
  void projectClosed();
}
-keep class com.eddysystems.eddy.actions.DumpEnvironment,com.eddysystems.eddy.actions.NextSuggestion,com.eddysystems.eddy.actions.PrevSuggestion,com.eddysystems.eddy.actions.Reinit {
  <init>(...);
  public void actionPerformed(...);
}

### Ignore various warnings and notes

# Base
-dontwarn javax.**
-dontwarn scala.**
-dontnote scala.**
-dontnote String

# Top level packages
-dontwarn sun.misc.**
-dontwarn org.xml.**
-dontwarn org.w3c.**
-dontwarn org.apache.**
-dontnote org.apache.**
-dontwarn com.amazonaws.** 
-dontnote com.amazonaws.**
-dontwarn com.fasterxml.**
-dontnote com.fasterxml.**
-dontwarn gnu.trove.**
-dontwarn org.joda.**
-dontnote org.joda.**

# IntelliJ
-dontwarn com.intellij.**
-dontwarn org.jetbrains.**

# Missing methods of base classes
-dontwarn com.eddysystems.eddy.EddyFileListener$EddyThread
-dontwarn com.eddysystems.eddy.engine.JavaEnvironment$NoJDKError
-dontwarn com.eddysystems.eddy.engine.Place$UnexpectedContainerError
-dontwarn com.eddysystems.eddy.actions.EddyAction$1
-dontwarn com.eddysystems.eddy.engine.Eddy$1$1
-dontwarn com.eddysystems.eddy.engine.EddyPsiListener$ElemType

# Weird scala issues
-dontwarn tarski.Semantics$$anonfun$denoteStmt$27$$anonfun$tarski$Semantics$$anonfun$$rest$1$1
