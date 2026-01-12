package kyo.arc2.grid

import kyo.ai.json.Json

enum Connectivity derives CanEqual, Json:
    case FourConnected, EightConnected

enum Axis derives CanEqual, Json:
    case Horizontal, Vertical

enum Rotation derives CanEqual, Json:
    case R90, R180, R270

enum Shape derives CanEqual, Json:
    case Square, Rectangle, Line, Solid, Hollow, SinglePixel, LShape, TShape, Cross

enum Symmetry derives CanEqual, Json:
    case Vertical, Horizontal, Diagonal, AntiDiagonal

enum Position derives CanEqual, Json:
    case TouchesEdge, TouchesTop, TouchesBottom, TouchesLeft, TouchesRight, Centered, Corner,
        AlignedHorizontally, AlignedVertically

enum Label derives CanEqual, Json:
    case RepeatsHorizontally, RepeatsVertically, SingleColor, Sparse, Dense

enum Metric derives CanEqual, Json:
    case Euclidean, Manhattan, Chebyshev

enum Anchor derives CanEqual, Json:
    case TopLeft, Center

enum Direction derives CanEqual, Json:
    case Up, Down, Left, Right

enum Alignment derives CanEqual, Json:
    case AlignHorizontal, AlignVertical, AlignGrid

enum ObjectProperty derives CanEqual, Json:
    case Size, X, Y, Width, Height, Color, AspectRatio, PixelCount, DominantColor

enum LabelCategory derives CanEqual, Json:
    case Shape, Symmetry, Position

case class Mapping[A, B](from: A, to: B)
