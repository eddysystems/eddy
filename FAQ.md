## FAQ
======

### Why is eddy messing up my indentation?

Eddy inserts its fixes using IntelliJ's formatting settings, so if you are
seeing bad formatting it is likely that IntelliJ is incorrectly configured.
The settings can be adjusted in IntelliJ via `Preferences / Code Style / Java`. 
Note that changes made to the settings only take effects after a restart.

### Eddy and Other Languages

In order to suggest solutions, eddy scans libraries and project files for
classes. Eddy may not fully understand classes defined in non-Java files
interpreted by other IntelliJ plugins (for instance, the wonderful Scala
plugin). Therefore, eddy may not have suggestions for code involving classes
defined in languages other than Java.

