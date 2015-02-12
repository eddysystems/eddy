### What is eddy?

Eddy is a plugin for IntelliJ that helps you write code faster. It can auto-correct
broken Java to legal Java, correcting syntax as well as many common semantic errors.
No matter whether you're new to Java and figuring out its syntax, or whether you're
a seasoned Java programmer who wants to write shorthand, eddy is for you. Even
considering IntelliJ's pretty advanced inspection and correction features, you
will save a lot of time. Consider this input:

> TODO

IntelliJ highlighted an error, and you can hover over the line to find out what's
wrong. However, it won't offer to fix the problem, and correcting the mistakes requires
time and breaks your workflow.

Eddy on the other hand offers a solution immediately:

> TODO

Just hit Alt+Enter to accept it and continue working.

Eddy seamlessly integrates into IntelliJs user interface, and uses common shortcuts
that you're already used to.

But that's not all. With eddy, you can start writing shorthand, as long as it's
reasonably clear what you're trying to do. Eddy knows shorthands for many common
language constructs, for instance:

> for i in tokens

Eddy also does not insist on the details of Java syntax. For example, If you're
used to writing C++, you may be tempted to write

> Arrays.copyOf<T,U>(a, n, b)

instead of

> Arrays.<T,U>copyOf(a, n, b)

Eddy will sort this out for you. Eddy is a learning system, and there's no specific
set of things that it understands. Our philosophy is that if it was clear what you
are trying to say, Eddy should understand it. If eddy doesn't understand something,
you should [leave a suggestion](TODO).

## Examples

Here are a few examples of the kind of things eddy understands and fixes for you:

# Abbreviated control flow syntax

// TODO

# Abbreviated variable declarations

> x = 5 => int x = 5;
> x = Object() => Object x = new Object();

# Omitted qualifiers

> x = 5 => this.x = 5;
> s = "" => OuterClass.this.s = ""
> static_d = 0.0 => SomeClass.static_d = 0.0

# Typos

// TODO

# Rearranging arguments

// TODO

### Installing eddy

Eddy is plugin for the excellent [IntelliJ IDEA](TODO) Java IDE. It is compatible
with IntelliJ Idea 13 and 14. To install it, install IntelliJ if you don't have it
already, and then go to `Preferences / Plugins` and click `Browse Repositories`.
You can search for "eddy" or select it from the list. After a restart, eddy is
installed.

Eddy scans projects every time they're opened, a process that takes around 10-15
seconds. Of course, you can work while eddy is initializing in the background,
and eddy should be available even while it is still initializing.

### Interacting with eddy

We made it a priority to integrate as cleanly as possible into IntelliJ's user
interface. If you're used to working with IntelliJ, eddy will feel natural to you.

Eddy is only active in Java editor windows, and only inside of code blocks. It
does not currently offer suggestions for class, method, or field declarations,
or for import or package statements.

Whenever eddy is active and you move to a new line of code or type something,
eddy will process the current line of code (or, to be exact, the current line
or the current statement, whichever is larger). This happens in the background
and should not interfere with your editing. If eddy finds a solution, it will
offer an [intention action](TODO). If eddy finds only solutions that are
different than what you wrote, it also shows a [hint](TODO).

## Eddy hints

If eddy finds only solutions that are not the same as what's already there, it
will show a hint. Most commonly, you will see this kind of hint:

// TODO

This is eddy's way of telling you it has found a solution, and it's different
from what you wrote. If eddy has found only one solution, you can hit Alt+Enter
to immediately apply the solution. Your cursor will be at the same point in the
line as before.

If eddy found several possible solutions, the hint will say so, like this:

// TODO

Then, hitting Alt+Enter will open a menu with up to four possible solutions, from
which you can select one to apply.

If you have enabled the auto-apply feature in eddy's settings [link below](TODO),
and if eddy is very confident about its preferred solution, the hint will look
like this:

// TODO

Eddy will only show these hints if you are at the end of the line. If you hit
enter, the solution will be applied automatically, and your cursor will be in the
next line (as you would expect when you hit enter). If you don't want eddy's
suggestion, press escape. The hint will go away and enter will behave normally.

Eddy shows hints after you have edited the file, or if you moved to a different
line. It will not show hints if you move around within a line.

## Eddy's intention action

Even if eddy does not show a hint, you can still look at eddy's solutions through
IntelliJ's intention action system. Once IntelliJ's lightbulb shows

// TODO

hit Alt+Enter to access the intentions menu. Eddy offers two intentions. If it found
anything, you will see the "eddy thinks..." action (if eddy found a unique solution,
it will tell you here). Selecting it has the same effect as hitting Alt+Enter when a
hint is shown.

Whenever eddy is available, you will also see the "suggest solution" action. It's
described below.

## Suggesting a solution for eddy

Whenever eddy doesn't find what you're looking for, you can suggest the correct
solution using the ["suggest solution"](TODO) intention action. This will bring up
this dialog:

// TODO

It shows the input eddy was processing, and the list of abbreviated solutions it found.
Each solution comes with a score (roughly, a probability). If you click on any solution,
an expanded version (the text that would have been inserted) is shown in the text field
below.

You can enter your preferred solution in the text field on the bottom of the dialog.
It's just text, so feel free to add any information you feel would be helpful to
understand why eddy's solution is not the best one, for example, information about
classes used in the line, your intentions when writing the original line, or maybe
some surrounding code that eddy should have used as input but didn't.

### Configuring eddy

You can access eddy's configuration in IntelliJ's preferences dialog. In IntelliJ 13,
there is an `Eddy` entry in the `IDE Settings` section, in IntelliJ 14, `Eddy` is under
`Other Settings`. In either case, searching for `Eddy` in the search box will find it
quickly. Once you bring up the configuration dialog, it looks something like this:

// TODO (with labels)

You can configure two main settings here: 1-3 control when eddy shows auto-apply hints
(suggestions that are applied on enter, not alt+enter), and 4-5 control when eddy stops
looking for more solutions.

## Configuring auto-apply

The checkbox 1 turns auto-apply on or off. If it is on, auto-apply will only be offered
if the probability of the best found solution is above the threshold given in 2, *and*
if there is either no other solution, or if the probability of the second best solution
is worse by at least the factor given in 3.

The default values are fairly conservative. We want to avoid showing auto-apply hints
in situations where we could be wrong. If you find yourself often accepting the first
hint, you may want to reduce the numbers in 2 and 3 to better reflect your preferences.
The more you get used to eddy, the more you will use its abbreviation and shortcut
features, and you will want to immediately accept suggestions with lower absolute
probabilities.

Eddy will stop looking for solutions once their probability falls below the threshold
given in 4. To avoid not seeing more esoteric solutions, absolute threshold in 4 should
not be set too high. We frequently see useful solutions with probabilities barely above
1e-4, depending how much you use eddy's more advanced fixes. The field 5 sets a relative
bound: eddy will stop looking once it knows there are no more solutions whose
probabilities are above the probability of the best found solution times the factor
given in 5.
