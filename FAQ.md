## FAQ
======

### How does eddy work?

This question is a little much for the FAQ, but we will write blog posts that
explain the inner workings in more detail.

In broad strokes: Eddy uses structured AI to find out what you mean when you
write incomplete or broken code. It knows a lot about Java and about code, and
it uses that knowledge to compute a probabilistic model of what you meant to write
given what you actually typed.

Specifically, eddy parses what you wrote using an ambiguous grammar, and then
tries to find an interpretation that makes sense (type-checks, and matches known
programming patterns). Eddy controls the exponential complexity of this process
by looking at code much like a human would: it first looks at the most likely
possibilities and explores ways to make them work. Only if simple fixes fail
does it explore more esoteric solutions.

### Eddy doesn't understand me!

Sometimes, eddy will not understand something that should be obvious. It's
early days, and eddy will only parse statements inside a code block. It also
does not understand all Java constructs. In particular, it does not understand
statements involving an anonymous class declaration or lambda expression, a
method ref, or switch/case statements (although it understands statements inside
those fine).

The culprit can also be a class which is not defined in Java, which eddy doesn't
fully understand. A statement using such a class may not be understood by eddy.

Or maybe, what you were writing is not quite legal, for instance,
you may be trying to use an inaccessible field or method. Eddy tries hard
to only produce legal code, and it may remain silent such cases.

Eddy isn't perfect, but it is set up to learn over time. If you think that eddy
should understand you and it doesn't, please submit a suggestion using the
`suggest solution` intention action. Your suggestions help to improve eddy
faster.

### I think eddy made a mistake!

Please tell us what the correct solution would have been using the
`suggest solution` intention action. While eddy will learn from
general usage, suggestions that point out specific problems are valuable to us
and help tremendously in improving eddy faster.

### Why is eddy messing up my indentation?

Eddy inserts its fixes using IntelliJ's formatting settings, so if you are
seeing bad formatting it is likely that IntelliJ is incorrectly configured.
The settings can be adjusted in IntelliJ via `Preferences / Code Style / Java`. 
Note that changes made to the settings only take effects after a restart.

### Eddy and other languages

In order to suggest solutions, eddy scans libraries and project files for
classes. Eddy may not fully understand classes defined in non-Java files
interpreted by other IntelliJ plugins (for instance, the Scala plugin).
Therefore, eddy may not have suggestions for code involving classes defined
in languages other than Java.

### Why is eddy "phoning home"?

Every time you use eddy, it understand a little bit more about how code works.
We collect and process statistics about the code you write, the suggestions eddy
makes, and which suggestions you accept. These data are collected anonymously,
transmitted securely, they are not shared with third parties, and we use them only
to improve eddy. You can read more about how we use your data in our [privacy policy](TODO).

### Can I opt out of knowledge collection?

Not yet. In the free version of eddy, you get to use eddy, and eddy gets to learn
from how you use it. In the future, we will offer paid subscriptions in which you can
opt out of data collection entirely or restrict the use of your inputs only to your
own eddy account.
