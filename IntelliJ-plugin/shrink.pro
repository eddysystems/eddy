-injars eddy.zip
-outjars eddy.jar 

### Keep a few things

-keepclasseswithmembernames class com.eddysystems.eddy.EddyPlugin
-keepclasseswithmembernames class com.eddysystems.eddy.actions.EddyAction
-keepclasseswithmembernames class com.eddysystems.eddy.actions.DumpEnvironment
-keepclasseswithmembernames class com.eddysystems.eddy.actions.NextSuggestion
-keepclasseswithmembernames class com.eddysystems.eddy.actions.PrevSuggestion
-keepclasseswithmembernames class com.eddysystems.eddy.actions.Reinit

### Ignore various warnings and notes

# Base
-dontwarn java.**
-dontwarn javax.**
-dontwarn scala.**
-dontnote scala.**

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
