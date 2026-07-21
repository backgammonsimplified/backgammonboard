# Move Overlay Rendering Contract

**Contract version:** 1.0  
**Status:** planned, not implemented on the move-application branch

## Board-scale resilience

Point outlines must remain visible at thumbnail and mobile widths.

Implementation must use one or both of:

```text
stroke width scaled with board dimensions
minimum rendered point-outline stroke width
```

The minimum must prevent point outlines from disappearing at supported small
output sizes.

## Default arrow construction

A move arrow must not be a plain single-colour stroke. Every arrow uses three
stacked paths in this order:

```text
1. dark outer halo
2. light inner halo
3. semantic coloured arrow
```

The layered arrow must remain visible while crossing:

```text
cream playing fields
blue points
tan points
White checkers
Black checkers
```

Visibility must be achieved through layered strokes and markers, not by
changing the frozen board palette.

## Ordered multi-part moves

Compound and multi-part plays must preserve `step_id` order using one or both
of:

```text
order numbers
distinct endpoint markers
```

The chosen system must distinguish repeated movement of one checker from
independent checker movements.

## Required future cases

```text
point-to-point
compound chain
repeated movement
bar entry
bearing off
confirmed hit
four-part double
dense checker stacks
small supported output size
standard supported output size
```

## Deferred implementation pipeline

```text
applied atomic steps
-> perspective-aware overlay coordinates
-> layered paths and endpoint markers
-> snapshot fixtures
-> manual visual review
```
