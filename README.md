# A Haskell implementation of Babbage's Analytical Engine

## Programs

A program is a folder that contains a `numbers.pc`, a `loadstore.pc`, and an `operations.pc`. These are "PunchCard" files, and should consist of one or more "PunchCards", separated by a newline and a `--`. Information on PunchCard formats can be found [here](#punchcard-formatting)

## PunchCard Formatting

### Operations

A operation may be one of the operations defined below. Each operation has two columns, and five rows. Empty space should be filled with the appropriate number of spaces. 

#### Add

```
*




```

#### Subtract

```
 *




```

#### Multiply

```

 *



```

#### Divide

```

  *



```

#### Load (preserving)

```


 *


```

#### Load (zeroing)

```


  *


```

#### Store

```



 *

```

#### Store (primed)

```



  *

```

#### Move Forwards

```
*



 *
```

#### Move Backwards

```
 *



 *
```

#### Conditional Move Forwards

Moves the operation sequence forwards by N steps *only if* the run-up lever is active. N is specified by a Number card linked via a Load/Store card. The run-up lever state is not implemented as a physical lever but as an internal boolean flag.
Card Format (Row 1: `**`, other rows empty):
```
**




```

#### Conditional Move Backwards

Moves the operation sequence backwards by N steps *only if* the run-up lever is active. N is specified by a Number card linked via a Load/Store card.
Card Format (Row 2: `**`, other rows empty):
```

**



```

#### Set Run-Up Lever

Activates the run-up lever (sets its internal state to true). This operation consumes a dummy variable from the Load/Store cards.
Card Format (Row 1: `* `, Row 3: `**`, other rows empty):
```
*

**


```

#### Clear Run-Up Lever

Deactivates the run-up lever (sets its internal state to false). This operation consumes a dummy variable from the Load/Store cards.
Card Format (Row 1: ` *`, Row 3: `**`, other rows empty):
```
 *

**


```

### Numbers

Each number is a signed 10 digit number. The negative sign is represented by a "punched" hole in the first column. Each other column should have one hole, representing a digit between 0 and 9.

```
0 ***********
1  **********
2  **********
3  **********
4  **********
5  **********
6  **********
7  **********
8  **********
9  **********
```

### Load & Store Variables

Each variable is an unsigned 3 digit number representing a location in the Store memory. The format for these variables is similar to the Number card [above](#numbers). 

```
0 ***
1 ***
2 ***
3 ***
4 ***
5 ***
6 ***
7 ***
8 ***
9 ***
```
