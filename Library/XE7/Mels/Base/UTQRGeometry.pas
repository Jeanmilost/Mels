 {*****************************************************************************}
 { ==> UTQRGeometry -----------------------------------------------------------}
 {*****************************************************************************}
 { Description : Provides base classes for geometric calculations, as e.g.     }
 {               vectors, matrices, plans, ...                                 }
 { Developer   : Jean-Milost Reymond                                           }
 { Copyright   : (c) 2015 - 2016, this file is part of the Mels library        }
 {*****************************************************************************}
 { MIT License                                                                 }
 {                                                                             }
 { Copyright (c) 2015 - 2016, the Mels library                                 }
 {                                                                             }
 { Permission is hereby granted, free of charge, to any person obtaining a     }
 { copy of this software and associated documentation files (the "Software"),  }
 { to deal in the Software without restriction, including without limitation   }
 { the rights to use, copy, modify, merge, publish, distribute, sublicense,    }
 { and/or sell copies of the Software, and to permit persons to whom the       }
 { Software is furnished to do so, subject to the following conditions:        }
 {                                                                             }
 { The above copyright notice and this permission notice shall be included in  }
 { all copies or substantial portions of the Software.                         }
 {                                                                             }
 { THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR  }
 { IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,    }
 { FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE }
 { AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER      }
 { LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING     }
 { FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER         }
 { DEALINGS IN THE SOFTWARE.                                                   }
 {*****************************************************************************}

unit UTQRGeometry;

interface

uses System.SysUtils,
     System.Math;

type

  {$REGION 'TQRVector2D'}

  {**
  * Vector 2D
  *}
  TQRVector2D = record
  private
    FX: Double; // x coordinate for the 2D vector
    FY: Double; // y coordinate for the 2D vector

  public
    {**
    * Constructor
    *@param X - vector x coordinate
    *@param Y - vector y coordinate
    *}
    constructor Create(const X, Y: Double); overload;

    {**
    * Constructor
    *@param Other - other vector to copy from
    *}
    constructor Create(const Other: TQRVector2D); overload;

    { Basic functions }
    procedure Assign(const Other: TQRVector2D);                              inline;
    function  Add(const Value: Double): TQRVector2D;               overload; inline;
    function  Add(const Other: TQRVector2D): TQRVector2D;          overload; inline;
    function  Invert: TQRVector2D;                                 overload; inline;
    function  Sub(const Value: Double): TQRVector2D;               overload; inline;
    function  Sub(const Other: TQRVector2D): TQRVector2D;          overload; inline;
    function  Mul(const Value: Double): TQRVector2D;               overload; inline;
    function  Mul(const Other: TQRVector2D): TQRVector2D;          overload; inline;
    function  Divide(const Value: Double): TQRVector2D;            overload; inline;
    function  Divide(const Other: TQRVector2D): TQRVector2D;       overload; inline;
    function  AddAndAssign(const Value: Double): TQRVector2D;      overload; inline;
    function  AddAndAssign(const Other: TQRVector2D): TQRVector2D; overload; inline;
    function  SubAndAssign(const Value: Double): TQRVector2D;      overload; inline;
    function  SubAndAssign(const Other: TQRVector2D): TQRVector2D; overload; inline;
    function  MulAndAssign(const Value: Double): TQRVector2D;      overload; inline;
    function  MulAndAssign(const Other: TQRVector2D): TQRVector2D; overload; inline;
    function  DivAndAssign(const Value: Double): TQRVector2D;      overload; inline;
    function  DivAndAssign(const Other: TQRVector2D): TQRVector2D; overload; inline;
    function  IsEqual(const Other: TQRVector2D): Boolean;                    inline;
    function  Differs(const Other: TQRVector2D): Boolean;                    inline;

    {**
    * Gets the vector length
    *@return vector length
    *}
    function Length: Double; inline;

    {**
    * Normalizes the vector
    *@return normalized vector
    *}
    function Normalize: TQRVector2D; inline;

    {**
    * Calculates the cross product between 2 vectors
    *@param Vector - vector with which cross product is calculated
    *@return cross product
    *}
    function Cross(const Vector: TQRVector2D): TQRVector2D; inline;

    {**
    * Calculates the dot product between 2 vectors
    *@param Vector - vector with which dot product is calculated
    *@return dot product
    *}
    function Dot(const Vector: TQRVector2D): Double; inline;

    {**
    * Calculates interpolation vector between 2 vectors
    *@param Other - other vector to interpolate with
    *@param Position - interpolation position, in percent (between 0.0 and 1.0)
    *@return interpolation vector
    *}
    function Interpolate(const Other: TQRVector2D; const Position: Double): TQRVector2D; inline;

    { Properties }
    property X: Double read FX write FX;
    property Y: Double read FY write FY;
  end;

  PQRVector2D = ^TQRVector2D;

  {$ENDREGION}

  {$REGION 'TQRVector3D'}

  {**
  * Vector 3D
  *}
  TQRVector3D = record
  private
    FX: Double; // x coordinate for the 3D vector
    FY: Double; // y coordinate for the 3D vector
    FZ: Double; // z coordinate for the 3D vector

  public
    {**
    * Constructor
    *@param X - vector x coordinate
    *@param Y - vector y coordinate
    *@param Z - vector z coordinate
    *}
    constructor Create(const X, Y, Z: Double); overload;

    {**
    * Constructor
    *@param Other - other vector to copy from
    *}
    constructor Create(const Other: TQRVector3D); overload;

    { Basic functions }
    procedure Assign(const Other: TQRVector3D);                              inline;
    function  Add(const Value: Double): TQRVector3D;               overload; inline;
    function  Add(const Other: TQRVector3D): TQRVector3D;          overload; inline;
    function  Invert: TQRVector3D;                                 overload; inline;
    function  Sub(const Value: Double): TQRVector3D;               overload; inline;
    function  Sub(const Other: TQRVector3D): TQRVector3D;          overload; inline;
    function  Mul(const Value: Double): TQRVector3D;               overload; inline;
    function  Mul(const Other: TQRVector3D): TQRVector3D;          overload; inline;
    function  Divide(const Value: Double): TQRVector3D;            overload; inline;
    function  Divide(const Other: TQRVector3D): TQRVector3D;       overload; inline;
    function  AddAndAssign(const Value: Double): TQRVector3D;      overload; inline;
    function  AddAndAssign(const Other: TQRVector3D): TQRVector3D; overload; inline;
    function  SubAndAssign(const Value: Double): TQRVector3D;      overload; inline;
    function  SubAndAssign(const Other: TQRVector3D): TQRVector3D; overload; inline;
    function  MulAndAssign(const Value: Double): TQRVector3D;      overload; inline;
    function  MulAndAssign(const Other: TQRVector3D): TQRVector3D; overload; inline;
    function  DivAndAssign(const Value: Double): TQRVector3D;      overload; inline;
    function  DivAndAssign(const Other: TQRVector3D): TQRVector3D; overload; inline;
    function  IsEqual(const Other: TQRVector3D): Boolean;                    inline;
    function  Differs(const Other: TQRVector3D): Boolean;                    inline;

    {**
    * Gets the vector length
    *@return vector length
    *}
    function Length: Double; inline;

    {**
    * Normalizes the vector
    *@return normalized vector
    *}
    function Normalize: TQRVector3D; inline;

    {**
    * Calculates the cross product between 2 vectors
    *@param Vector - vector with which cross product is calculated
    *@return cross product
    *}
    function Cross(const Vector: TQRVector3D): TQRVector3D; inline;

    {**
    * Calculates the dot product between 2 vectors
    *@param Vector - vector with which dot product is calculated
    *@return dot product
    *}
    function Dot(const Vector: TQRVector3D): Double; inline;

    {**
    * Calculates interpolation vector between 2 vectors
    *@param Other - other vector to interpolate with
    *@param Position - interpolation position, in percent (between 0.0 and 1.0)
    *@return interpolation vector
    *}
    function Interpolate(const Other: TQRVector3D; const Position: Double): TQRVector3D; inline;

    { Properties }
    property X: Double read FX write FX;
    property Y: Double read FY write FY;
    property Z: Double read FZ write FZ;
  end;

  PQRVector3D = ^TQRVector3D;

  {$ENDREGION}

  {$REGION 'TQRPlane'}

  {**
  * Plane
  *}
  TQRPlane = record
  private
    FA: Double; // a coordinate for the plane
    FB: Double; // b coordinate for the plane
    FC: Double; // c coordinate for the plane
    FD: Double; // d coordinate for the plane

  public
    {**
    * Constructor
    *@param A - plane a coordinate (relative to formula aX + bY + cZ + d = 0)
    *@param B - plane b coordinate (relative to formula aX + bY + cZ + d = 0)
    *@param C - plane c coordinate (relative to formula aX + bY + cZ + d = 0)
    *@param D - plane d coordinate (relative to formula aX + bY + cZ + d = 0)
    *}
    constructor Create(const A, B, C, D: Double); overload;

    {**
    * Constructor
    *@param Other - other plane to copy from
    *}
    constructor Create(const Other: TQRPlane); overload;

    { Basic functions }
    procedure Assign(const Other: TQRPlane);           inline;
    function  Invert: TQRPlane;                        inline;
    function  IsEqual(const Other: TQRPlane): Boolean; inline;
    function  Differs(const Other: TQRPlane): Boolean; inline;

    {**
    * Calculates distance to plane
    *@param point - point from which the distance must be calculated
    *@return distance to plane
    *}
    function DistanceTo(const Point: TQRVector3D): Double; inline;

    {**
    * Checks if plane intersects line and calculates intersection point
    *@param V1 - line start
    *@param V2 - line end
    *@param[out] P - calculated point on plane
    *@return true if plane intersects line, otherwise false
    *}
    function IntersectLine(const V1, V2: TQRVector3D; out P: TQRVector3D): Boolean; inline;

    {**
    * Checks if plane intersects ray and calculates intersection point
    *@param Rp - ray start point
    *@param Rd - ray direction
    *@param[out] P - calculated point on plane
    *@return true if plane intersects ray, otherwise false
    *}
    function IntersectRay(const Rp, Rd: TQRVector3D; out P: TQRVector3D): Boolean; inline;

    {**
    * Compare plane with the given plane using the given tolerance
    *@param Other - Other plane to compare
    *@param Tolerance - tolerance for comparison
    *@return true if planes are equals in the limits of the given tolerance,
    *         otherwise false
    *}
    function Compare(const Other: TQRPlane; const Tolerance: Double): Boolean; inline;

    {**
    * Calculates a plane using 3 vertex
    *@param V1 - Value of the first vertex
    *@param V2 - Value of the second vertex
    *@param V3 - Value of the thrid vertex
    *@return the built plane
    *}
    class function FromPoints(const V1, V2, V3: TQRVector3D): TQRPlane; static; inline;

    {**
    * Calculates a plane using a point and a normal
    *@param Point - a point belongs to the plane
    *@param Normal - normal of the plane
    *@return the built plane
    *}
    class function FromPointNormal(const Point, Normal: TQRVector3D): TQRPlane; static; inline;

    { Properties }
    property A: Double read FA write FA;
    property B: Double read FB write FB;
    property C: Double read FC write FC;
    property D: Double read FD write FD;
  end;

  PQRPlane = ^TQRPlane;

  {$ENDREGION}

  {$REGION 'TQRMatrix4x4'}

  {**
  * 4x4 matrix
  *}
  TQRMatrix4x4 = record
  private
    FTable: array[0..3] of array [0..3] of Double; // 4x4 matrix array

    {**
    * Gets matrix item at index
    *@param Index - index
    *@return item
    *@throw exception if index is out of bounds
    *}
    function  GetItem(Index: NativeInt): Double; overload; inline;

    {**
    * Sets matrix item at index
    *@param Index - index
    *@param Value - Value to set
    *@throw exception if index is out of bounds
    *}
    procedure SetItem(Index: NativeInt; Value: Double); overload; inline;

    {**
    * Gets matrix item from table
    *@param X - table x position
    *@param Y - table y position
    *@return item
    *@throw exception if positions are out of bounds
    *}
    function  GetTableItem(X, Y: NativeInt): Double; overload; inline;

    {**
    * Sets matrix item to table
    *@param X - table x position
    *@param Y - table y position
    *@param Value - Value to set
    *@throw exception if positions are out of bounds
    *}
    procedure SetTableItem(X, Y: NativeInt; Value: Double); overload; inline;

  public
    {**
    * Constructor
    *@param _11 - [0][0] matrix table Value
    *@param _12 - [1][0] matrix table Value
    *@param _13 - [2][0] matrix table Value
    *@param _14 - [3][0] matrix table Value
    *@param _21 - [0][1] matrix table Value
    *@param _22 - [1][1] matrix table Value
    *@param _23 - [2][1] matrix table Value
    *@param _24 - [3][1] matrix table Value
    *@param _31 - [0][2] matrix table Value
    *@param _32 - [1][2] matrix table Value
    *@param _33 - [2][2] matrix table Value
    *@param _34 - [3][2] matrix table Value
    *@param _41 - [0][3] matrix table Value
    *@param _42 - [1][3] matrix table Value
    *@param _43 - [2][3] matrix table Value
    *@param _44 - [3][3] matrix table Value
    *}
    constructor Create(const _11, _12, _13, _14,
                             _21, _22, _23, _24,
                             _31, _32, _33, _34,
                             _41, _42, _43, _44: Double); overload;

    {**
    * Constructor
    *@param Other - other matrix to copy from
    *}
    constructor Create(const Other: TQRMatrix4x4); overload;

    {**
    * Gets record initialized with default Values
    *@return record initialized with default Values
    *}
    class function GetDefault: TQRMatrix4x4; inline; static;

    { Basic functions }
    procedure Assign(const Other: TQRMatrix4x4);
    function  IsEqual(const Other: TQRMatrix4x4): Boolean;
    function  Differs(const Other: TQRMatrix4x4): Boolean; inline;

    {**
    * Checks if matrix is an identity matrix
    *@return true if matrix is an identity matrix, otherwise false
    *}
    function IsIdentity: Boolean; inline;

    {**
    * Gets matrix determinant
    *@return matrix determinant
    *}
    function Determinant: Double; inline;

    {**
    * Inverses the matrix
    *@param Determinant - determinant
    *@return inverse of the matrix
    *}
    function Inverse(out Determinant: Double): TQRMatrix4x4;

    {**
    * Multiplies matrix by anOther matrix
    *@param Other - other matrix to multiply with
    *@return multiplied resulting matrix
    *}
    function Multiply(const Other: TQRMatrix4x4): TQRMatrix4x4;

    {**
    * Translates matrix
    *@param T - translation vector
    *@return copy of translated matrix
    *}
    function Translate(const T: TQRVector3D): TQRMatrix4x4; inline;

    {**
    * Rotates matrix
    *@param Angle - rotation angle in radians
    *@param R - rotation direction (e.g. [0.0f, 0.0f, 1.0f] for a z-axis rotation)
    *@return copy of rotated matrix
    *@note rotation direction vector should be normalized before calling
    *      this function
    *}
    function Rotate(const Angle: Double; const R: TQRVector3D): TQRMatrix4x4; inline;

    {**
    * Scales matrix
    *@param S - scale vector
    *@return copy of scaled matrix
    *}
    function Scale(const S: TQRVector3D): TQRMatrix4x4; inline;

    {**
    * Swaps matrix lines and columns
    *@return swapped matrix
    *}
    function Swap: TQRMatrix4x4; inline;

    {**
    * Transform a vector by applying the matrix
    *@param Vector - vector to transform
    *@return transformed vector
    *}
    function Transform(const Vector: TQRVector3D): TQRVector3D; inline;

    {**
    * Gets table pointer
    *@return pointer
    *}
    function GetPtr: PDouble; inline;

    {**
    * Gets an identity matrix
    *@return identity matrix
    *}
    class function Identity: TQRMatrix4x4; static; inline;

    { Properties }
    property Item[Index: NativeInt]: Double read GetItem      write SetItem;
    property Table[X, Y: NativeInt]: Double read GetTableItem write SetTableItem;
    property Ptr: PDouble                   read GetPtr;
  end;

  PQRMatrix4x4 = ^TQRMatrix4x4;

  {$ENDREGION}

    {$REGION 'Quaternion'}

    {**
    * Quaternion
    *}
    TQRQuaternion = record
        private
            FX: Double; // x coordinate for the quaternion
            FY: Double; // y coordinate for the quaternion
            FZ: Double; // z coordinate for the quaternion
            FW: Double; // w coordinate for the quaternion

        public
            {**
            * Constructor
            *@param x - vector x Value
            *@param y - vector y Value
            *@param z - vector z Value
            *@param w - vector w Value
            *}
            constructor Create(x, y, z, w: Double); overload;

            {**
            * Constructor that creates a quaternion from an axis and an angle
            *@param vector - vector representing axis
            *@param angle - angle in radians
            *}
            constructor Create(const vector: TQRVector3D; angle: Double); overload;

            {**
            * Constructor that creates a quaternion from a 4x4 matrix
            *@param matrix - matrix
            *}
            constructor Create(const matrix: TQRMatrix4x4); overload;

            {**
            * Copy constructor
            *@param Other - Other quaternion to copy from
            *}
            constructor Create(const Other: TQRQuaternion); overload;

            { Basic functions }
            procedure Assign(const Other: TQRQuaternion);                                inline;
            function  Add(const Value: Double): TQRQuaternion;                 overload; inline;
            function  Add(const Other: TQRQuaternion): TQRQuaternion;          overload; inline;
            function  Invert: TQRQuaternion;                                   overload; inline;
            function  Sub(const Value: Double): TQRQuaternion;                 overload; inline;
            function  Sub(const Other: TQRQuaternion): TQRQuaternion;          overload; inline;
            function  Mul(const Value: Double): TQRQuaternion;                 overload; inline;
            function  Mul(const Other: TQRQuaternion): TQRQuaternion;          overload; inline;
            function  Divide(const Value: Double): TQRQuaternion;              overload; inline;
            function  Divide(const Other: TQRQuaternion): TQRQuaternion;       overload; inline;
            function  AddAndAssign(const Value: Double): TQRQuaternion;        overload; inline;
            function  AddAndAssign(const Other: TQRQuaternion): TQRQuaternion; overload; inline;
            function  SubAndAssign(const Value: Double): TQRQuaternion;        overload; inline;
            function  SubAndAssign(const Other: TQRQuaternion): TQRQuaternion; overload; inline;
            function  MulAndAssign(const Value: Double): TQRQuaternion;        overload; inline;
            function  MulAndAssign(const Other: TQRQuaternion): TQRQuaternion; overload; inline;
            function  DivAndAssign(const Value: Double): TQRQuaternion;        overload; inline;
            function  DivAndAssign(const Other: TQRQuaternion): TQRQuaternion; overload; inline;
            function  IsEqual(const Other: TQRQuaternion): Boolean;                      inline;
            function  Differs(const Other: TQRQuaternion): Boolean;                      inline;

            {**
            * Calculates the norm of the quaternion
            *@return the norm of the quaternion
            *}
            function Norm: Double; inline;

            {**
            * Gets the quaternion length
            *@return the quaternion length
            *}
            function Length: Double; inline;

            {**
            * Normalizes the vector
            *@return normalized vector
            *}
            function Normalize: TQRQuaternion; inline;

            {**
            * Calculates the dot product between 2 quaternions
            *@param q - quaternion with which dot product is calculated
            *@return dot product
            *}
            function Dot(const q: TQRQuaternion): Double; inline;

            {**
            * Scales the quaternion
            *@param s - scale factor to apply
            *@return scaled quaternion
            *}
            function Scale(s: Double): TQRQuaternion; inline;

            {**
            * Conjugates quaternion
            *@return the conjugate of the quaternion
            *}
            function Conjugate: TQRQuaternion; inline;

            {**
            * Inverse quaternion
            *@return inverted quaternion
            *}
            function Inverse: TQRQuaternion; inline;

            {**
            * Gets the spherical linear interpolated quaternion between 2 quaternions
            *@param Other - Other quaternion to interpolate with
            *@param p - interpolation position, in percent (between 0.0f and 1.0f)
            *@return the spherical linear interpolated quaternion
            *}
            function Slerp(const Other: TQRQuaternion; p: Double): TQRQuaternion; inline;

            {**
            * Rotates a vector by the quaternion
            *@param vector - vector to rotate
            *@return rotated vector
            *}
            function Rotate(const vector: TQRVector3D): TQRVector3D; inline;

            {**
            * Gets matrix from quaternion
            *@return matrix
            *}
            function GetMatrix: TQRMatrix4x4; inline;


            { Properties }
            property X: Double read FX write FX;
            property Y: Double read FY write FY;
            property Z: Double read FZ write FZ;
            property W: Double read FW write FW;
    end;

    PQRQuaternion = ^TQRQuaternion;

    {$ENDREGION}

  {**
  * Ray
  *}
    TQRRay = class
        protected
            FPos:    TQRVector3D;
            FDir:    TQRVector3D;
            m_InvDir: TQRVector3D;

            {**
            * Gets position
            *@return position
            *}
            function GetPos: PQRVector3D; virtual;

            {**
            * Sets position
            *@param pPos - position
            *}
            procedure SetPos(const pPos: PQRVector3D); virtual;

            {**
            * Gets direction
            *@return direction
            *}
            function GetDir: PQRVector3D; virtual;

            {**
            * Sets direction
            *@param pDir - direction
            *}
            procedure SetDir(const pDir: PQRVector3D); virtual;

            {**
            * Gets inverted direction
            *@return inverted direction
            *}
            function GetInvDir: PQRVector3D; virtual;

        public
            {**
            * Constructor
            *}
            constructor Create; overload; virtual;

            {**
            * Constructor
            *@param pOther - Other ray to copy from
            *}
            constructor Create(const pOther: TQRRay); overload; virtual;

            { Properties }
            property Pos:    PQRVector3D read GetPos write SetPos;
            property Dir:    PQRVector3D read GetDir write SetDir;
            property InvDir: PQRVector3D read GetInvDir;
    end;

    {**
    * Class representing a polygon
    *}
    TQRPolygon = record
        private
            m_Vertex: array[0..2] of TQRVector3D;

            {**
            * Checks if a vector is between start and end limits
            *@param Value - Value to check
            *@param vStart - start limit
            *@param vEnd - end limit
            *@param tolerance - tolerance
            *@return true if Value is between limits, otherwise false
            *}
            function IsBetween(const Value, vStart, vEnd: TQRVector3D;
                                               tolerance: Double): Boolean; overload;

            {**
            * Checks if a Value is between start and end limits
            *@param Value - Value to check
            *@param lStart - start limit
            *@param lEnd - end limit
            *@param tolerance - tolerance
            *@return true if Value is between limits, otherwise false
            *}
            function IsBetween(const Value, lStart, lEnd, tolerance: Double): Boolean; overload;

        public
            {**
            * Constructor
            *@param vertex1 - first vertex of the polygon
            *@param vertex2 - second vertex of the polygon
            *@param vertex3 - third vertex of the polygon
            *}
            constructor Create(const vertex1, vertex2, vertex3: TQRVector3D);

            {**
            * Gets vertex at index
            *@param index - vertex index
            *@return corresponding vertex, nil if not found
            *}
             function GetVertex(index: Byte): TQRVector3D;

            {**
            * Sets vertex
            *@param index - vertex index to set
            *@param vertex - vertex Value
            *}
            procedure SetVertex(index: Byte; const vertex: TQRVector3D);

            {**
            * Gets first polygon vertex
            *@return first polygon vertex
            *}
            function GetVertex1: PQRVector3D;

            {**
            * Sets first polygon vertex
            *@param pVertex - first polygon vertex Value
            *}
            procedure SetVertex1(const pVertex: PQRVector3D);

            {**
            * Gets second polygon vertex
            *@return second polygon vertex
            *}
            function GetVertex2: PQRVector3D;

            {**
            * Sets second polygon vertex
            *@param pVertex - second polygon vertex Value
            *}
            procedure SetVertex2(const pVertex: PQRVector3D);

            {**
            * Gets third polygon vertex
            *@return third polygon vertex
            *}
            function GetVertex3: PQRVector3D;

            {**
            * Sets third polygon vertex
            *@param pVertex - third polygon vertex Value
            *}
            procedure SetVertex3(const pVertex: PQRVector3D);

            {**
            * Creates and returns a clone of the polygon
            *@return a clone of the polygon
            *@note The returned polygon should be deleted when useless
            *}
            function GetClone: TQRPolygon;

            {**
            * Applies the given matrix to the polygon
            *@param matrix - matrix to apply
            *@return transformed polygon
            *@note The returned polygon should be deleted when useless
            *}
            function ApplyMatrix(const matrix: TQRMatrix4x4): TQRPolygon;

            {**
            * Gets the polygon plane
            *@return the polygon plane
            *}
            function GetPlane: TQRPlane;

            {**
            * Calculates and returns the center point of the polygon
            *@return the center point of the polygon
            *}
            function GetCenter: TQRVector3D;

            {**
            * Checks if a point is inside polygon
            *@param x - point x coordinate
            *@param y - point y coordinate
            *@param z - point z coordinate
            *@return true if point is inside polygon, otherwise false
            *}
            function Inside(const x, y, z: Double): Boolean; overload;

            {**
            * Checks if a point is inside polygon
            *@param point - point coordinate
            *@return true if point is inside polygon, otherwise false
            *}
            function Inside(const point: TQRVector3D): Boolean; overload;

            { Properties }
            property Vertex1: PQRVector3D read GetVertex1 write SetVertex1;
            property Vertex2: PQRVector3D read GetVertex2 write SetVertex2;
            property Vertex3: PQRVector3D read GetVertex3 write SetVertex3;
    end;

    {**
    * Polygon list
    *}
    TQRPolygons = array of TQRPolygon;
    PQRPolygons = ^TQRPolygons;

    {**
    * 2D circle
    *}
    TQRCircle = record
        private
            FPos:    TQRVector2D;
            m_Radius: Double;

            {**
            * Gets circle center pos
            *@return sphere center pos
            *}
            function GetPos: PQRVector2D;

            {**
            * Sets circle center pos
            *@param pPos - sphere center pos
            *}
            procedure SetPos(const pPos: PQRVector2D);

        public
            { Properties }
            property Pos:    PQRVector2D read GetPos   write SetPos;
            property Radius: Double      read m_Radius write m_Radius;
    end;

    {**
    * 3D sphere
    *}
    TQRSphere = record
        private
            FPos:    TQRVector3D;
            m_Radius: Double;

            {**
            * Gets sphere center pos
            *@return sphere center pos
            *}
            function GetPos: PQRVector3D;

            {**
            * Sets sphere center pos
            *@param pPos - sphere center pos
            *}
            procedure SetPos(const pPos: PQRVector3D);

        public
            { Properties }
            property Pos:    PQRVector3D read GetPos   write SetPos;
            property Radius: Double      read m_Radius write m_Radius;
    end;

    {**
    * 2D rectangle
    *}
    TQRRect = record
        private
            FMin: TQRVector2D;
            FMax: TQRVector2D;

            {**
            * Gets rect min edge
            *@return box min edge
            *}
            function GetMin: PQRVector2D;

            {**
            * Sets rect min edge
            *@param pValue - box min edge
            *}
            procedure SetMin(const pValue: PQRVector2D);

            {**
            * Gets rect max edge
            *@return box max edge
            *}
            function GetMax: PQRVector2D;

            {**
            * Sets rect max edge
            *@param pValue - box max edge
            *}
            procedure SetMax(const pValue: PQRVector2D);

        public
            {**
            * Constructor
            *@param x - rect x position, i.e. the position of the left edge
            *@param y - rect y position, i.e. the position of the top edge
            *@param width - width
            *@param height - height
            *}
            constructor Create(const x, y, width, height: Double);

            {**
            * Gets width
            *@return width
            *}
            function GetWidth: Double;

            {**
            * Gets height
            *@return height
            *}
            function GetHeight: Double;

            { Properties }
            property Min:    PQRVector2D read GetMin write SetMin;
            property Max:    PQRVector2D read GetMax write SetMax;
            property Width:  Double      read GetWidth;
            property Height: Double      read GetHeight;
    end;

    {**
    * 3D aligned-axis box
    *}
    TQRBox = record
        private
            FMin: TQRVector3D;
            FMax: TQRVector3D;

            {**
            * Gets box min edge
            *@return box min edge
            *}
            function GetMin: PQRVector3D;

            {**
            * Sets box min edge
            *@param pValue - box min edge
            *}
            procedure SetMin(const pValue: PQRVector3D);

            {**
            * Gets box max edge
            *@return box max edge
            *}
            function GetMax: PQRVector3D;

            {**
            * Sets box max edge
            *@param pValue - box max edge
            *}
            procedure SetMax(const pValue: PQRVector3D);

        public
            {**
            * Cuts box on the longest axis
            *@param[in, out] leftBox - resulting left box
            *@param[in, out] rightBox - resulting right box
            *}
            procedure Cut(var leftBox: TQRBox; var rightBox: TQRBox);

            { Properties }
            property Min: PQRVector3D read GetMin write SetMin;
            property Max: PQRVector3D read GetMax write SetMax;
    end;

    PQRBox = ^TQRBox;

implementation
//------------------------------------------------------------------------------
{$REGION 'TQRVector2D'}
//------------------------------------------------------------------------------
constructor TQRVector2D.Create(const X, Y: Double);
begin
  FX := X;
  FY := Y;
end;
//------------------------------------------------------------------------------
constructor TQRVector2D.Create(const Other: TQRVector2D);
begin
  Assign(Other);
end;
//------------------------------------------------------------------------------
procedure TQRVector2D.Assign(const Other: TQRVector2D);
begin
  FX := Other.FX;
  FY := Other.FY;
end;
//------------------------------------------------------------------------------
function TQRVector2D.Add(const Value: Double): TQRVector2D;
begin
  result := TQRVector2D.Create(FX + Value, FY + Value);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Add(const Other: TQRVector2D): TQRVector2D;
begin
  result := TQRVector2D.Create(FX + Other.FX, FY + Other.FY);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Invert: TQRVector2D;
begin
  result := TQRVector2D.Create(-FX, -FY);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Sub(const Value: Double): TQRVector2D;
begin
  Result := TQRVector2D.Create(FX - Value, FY - Value);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Sub(const Other: TQRVector2D): TQRVector2D;
begin
  Result := TQRVector2D.Create(FX - Other.FX, FY - Other.FY);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Mul(const Value: Double): TQRVector2D;
begin
  Result := TQRVector2D.Create(FX * Value, FY * Value);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Mul(const Other: TQRVector2D): TQRVector2D;
begin
  Result := TQRVector2D.Create(FX * Other.FX, FY * Other.FY);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Divide(const Value: Double): TQRVector2D;
begin
  if (Value = 0.0) then
    raise Exception.Create('Division by 0 is prohibited');

  Result := TQRVector2D.Create(FX / Value, FY / Value);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Divide(const Other: TQRVector2D): TQRVector2D;
begin
  if (Other.FX = 0.0) then
    raise Exception.Create('Vector x axis - division by 0 is prohibited');

  if (Other.FY = 0.0) then
    raise Exception.Create('Vector y axis - division by 0 is prohibited');

  Result := TQRVector2D.Create(FX / Other.FX, FY / Other.FY);
end;
//------------------------------------------------------------------------------
function TQRVector2D.AddAndAssign(const Value: Double): TQRVector2D;
begin
  FX := FX + Value;
  FY := FY + Value;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector2D.AddAndAssign(const Other: TQRVector2D): TQRVector2D;
begin
  FX := FX + Other.FX;
  FY := FY + Other.FY;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector2D.SubAndAssign(const Value: Double): TQRVector2D;
begin
  FX := FX - Value;
  FY := FY - Value;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector2D.SubAndAssign(const Other: TQRVector2D): TQRVector2D;
begin
  FX := FX - Other.FX;
  FY := FY - Other.FY;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector2D.MulAndAssign(const Value: Double): TQRVector2D;
begin
  FX := FX * Value;
  FY := FY * Value;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector2D.MulAndAssign(const Other: TQRVector2D): TQRVector2D;
begin
  FX := FX * Other.FX;
  FY := FY * Other.FY;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector2D.DivAndAssign(const Value: Double): TQRVector2D;
begin
  if (Value = 0.0) then
    raise Exception.Create('Division by 0 is prohibited');

  FX := FX / Value;
  FY := FY / Value;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector2D.DivAndAssign(const Other: TQRVector2D): TQRVector2D;
begin
  if (Other.FX = 0.0) then
    raise Exception.Create('Vector x axis - division by 0 is prohibited');

  if (Other.FY = 0.0) then
    raise Exception.Create('Vector y axis - division by 0 is prohibited');

  FX := FX / Other.FX;
  FY := FY / Other.FY;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector2D.IsEqual(const Other: TQRVector2D): Boolean;
begin
  Result := ((FX = Other.FX) and (FY = Other.FY));
end;
//------------------------------------------------------------------------------
function TQRVector2D.Differs(const Other: TQRVector2D): Boolean;
begin
  Result := ((FX <> Other.FX) or (FY <> Other.FY));
end;
//------------------------------------------------------------------------------
function TQRVector2D.Length: Double;
begin
  Result := Sqrt((FX * FX) + (FY * FY));
end;
//------------------------------------------------------------------------------
function TQRVector2D.Normalize: TQRVector2D;
var
  Len: Double;
begin
  Len := Length;

  if (Len = 0.0) then
    Result := Default(TQRVector2D)
  else
    Result := TQRVector2D.Create((FX / Len), (FY / Len));
end;
//------------------------------------------------------------------------------
function TQRVector2D.Cross(const Vector: TQRVector2D): TQRVector2D;
begin
  Result := TQRVector2D.Create((FY * Vector.FX) - (Vector.FY * FX),
                               (FX * Vector.FY) - (Vector.FX * FY));
end;
//------------------------------------------------------------------------------
function TQRVector2D.Dot(const Vector: TQRVector2D): Double;
begin
  Result := ((FX * Vector.FX) + (FY * Vector.FY));
end;
//------------------------------------------------------------------------------
function TQRVector2D.Interpolate(const Other: TQRVector2D; const Position: Double): TQRVector2D;
begin
  // is position out of bounds? Limit to min or max Values in this case
  if (Position < 0.0) then
  begin
    Result := Self;
    Exit;
  end
  else
  if (Position > 1.0) then
  begin
    Result := Other;
    Exit;
  end;

  // calculate interpolation
  Result.FX := FX + Position * (Other.FX - FX);
  Result.FY := FY + Position * (Other.FY - FY);
end;
//------------------------------------------------------------------------------
{$ENDREGION}
//------------------------------------------------------------------------------
{$REGION 'TQRVector3D'}
//------------------------------------------------------------------------------
constructor TQRVector3D.Create(const X, Y, Z: Double);
begin
  FX := X;
  FY := Y;
  FZ := Z;
end;
//------------------------------------------------------------------------------
constructor TQRVector3D.Create(const Other: TQRVector3D);
begin
  Assign(Other);
end;
//------------------------------------------------------------------------------
procedure TQRVector3D.Assign(const Other: TQRVector3D);
begin
  FX := Other.FX;
  FY := Other.FY;
  FZ := Other.FZ;
end;
//------------------------------------------------------------------------------
function TQRVector3D.Add(const Value: Double): TQRVector3D;
begin
  Result := TQRVector3D.Create(FX + Value, FY + Value, FZ + Value);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Add(const Other: TQRVector3D): TQRVector3D;
begin
  Result := TQRVector3D.Create(FX + Other.FX, FY + Other.FY, FZ + Other.FZ);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Invert: TQRVector3D;
begin
  Result := TQRVector3D.Create(-FX, -FY, -FZ);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Sub(const Value: Double): TQRVector3D;
begin
  Result := TQRVector3D.Create(FX - Value, FY - Value, FZ - Value);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Sub(const Other: TQRVector3D): TQRVector3D;
begin
  Result := TQRVector3D.Create(FX - Other.FX, FY - Other.FY, FZ - Other.FZ);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Mul(const Value: Double): TQRVector3D;
begin
  Result := TQRVector3D.Create(FX * Value, FY * Value, FZ * Value);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Mul(const Other: TQRVector3D): TQRVector3D;
begin
  Result := TQRVector3D.Create(FX * Other.FX, FY * Other.FY, FZ * Other.FZ);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Divide(const Value: Double): TQRVector3D;
begin
  if (Value = 0.0) then
    raise Exception.Create('Division by 0 is prohibited');

  Result := TQRVector3D.Create(FX / Value, FY / Value, FZ / Value);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Divide(const Other: TQRVector3D): TQRVector3D;
begin
  if (Other.FX = 0.0) then
    raise Exception.Create('Vector x axis - division by 0 is prohibited');

  if (Other.FY = 0.0) then
    raise Exception.Create('Vector y axis - division by 0 is prohibited');

  if (Other.FZ = 0.0) then
    raise Exception.Create('Vector z axis - division by 0 is prohibited');

  Result := TQRVector3D.Create(FX / Other.FX, FY / Other.FY, FZ / Other.FZ);
end;
//------------------------------------------------------------------------------
function TQRVector3D.AddAndAssign(const Value: Double): TQRVector3D;
begin
  FX := FX + Value;
  FY := FY + Value;
  FZ := FZ + Value;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector3D.AddAndAssign(const Other: TQRVector3D): TQRVector3D;
begin
  FX := FX + Other.FX;
  FY := FY + Other.FY;
  FZ := FZ + Other.FZ;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector3D.SubAndAssign(const Value: Double): TQRVector3D;
begin
  FX := FX - Value;
  FY := FY - Value;
  FZ := FZ - Value;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector3D.SubAndAssign(const Other: TQRVector3D): TQRVector3D;
begin
  FX := FX - Other.FX;
  FY := FY - Other.FY;
  FZ := FZ - Other.FZ;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector3D.MulAndAssign(const Value: Double): TQRVector3D;
begin
  FX := FX * Value;
  FY := FY * Value;
  FZ := FZ * Value;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector3D.MulAndAssign(const Other: TQRVector3D): TQRVector3D;
begin
  FX := FX * Other.FX;
  FY := FY * Other.FY;
  FZ := FZ * Other.FZ;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector3D.DivAndAssign(const Value: Double): TQRVector3D;
begin
  if (Value = 0.0) then
    raise Exception.Create('Division by 0 is prohibited');

  FX := FX / Value;
  FY := FY / Value;
  FZ := FZ / Value;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector3D.DivAndAssign(const Other: TQRVector3D): TQRVector3D;
begin
  if (Other.FX = 0.0) then
    raise Exception.Create('Vector x axis - division by 0 is prohibited');

  if (Other.FY = 0.0) then
    raise Exception.Create('Vector y axis - division by 0 is prohibited');

  if (Other.FZ = 0.0) then
    raise Exception.Create('Vector z axis - division by 0 is prohibited');

  FX := FX / Other.FX;
  FY := FY / Other.FY;
  FZ := FZ / Other.FZ;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRVector3D.IsEqual(const Other: TQRVector3D): Boolean;
begin
  Result := ((FX = Other.FX) and (FY = Other.FY) and (FZ = Other.FZ));
end;
//------------------------------------------------------------------------------
function TQRVector3D.Differs(const Other: TQRVector3D): Boolean;
begin
  Result := ((FX <> Other.FX) or (FY <> Other.FY) or (FZ <> Other.FZ));
end;
//------------------------------------------------------------------------------
function TQRVector3D.Length: Double;
begin
  Result := Sqrt((FX * FX) + (FY * FY) + (FZ * FZ));
end;
//------------------------------------------------------------------------------
function TQRVector3D.Normalize: TQRVector3D;
var
  Len: Double;
begin
  Len := Length;

  if (Len = 0.0) then
    Result := Default(TQRVector3D)
  else
    Result := TQRVector3D.Create((FX / Len), (FY / Len), (FZ / Len));
end;
//------------------------------------------------------------------------------
function TQRVector3D.Cross(const Vector: TQRVector3D): TQRVector3D;
begin
  Result := TQRVector3D.Create((FY * Vector.FZ) - (Vector.FY * FZ),
                               (FZ * Vector.FX) - (Vector.FZ * FX),
                               (FX * Vector.FY) - (Vector.FX * FY));
end;
//------------------------------------------------------------------------------
function TQRVector3D.Dot(const vector: TQRVector3D): Double;
begin
  Result := ((FX * Vector.FX) + (FY * Vector.FY) + (FZ * Vector.FZ));
end;
//------------------------------------------------------------------------------
function TQRVector3D.Interpolate(const Other: TQRVector3D; const Position: Double): TQRVector3D;
begin
  // is position out of bounds? Limit to min or max Values in this case
  if (Position < 0.0) then
  begin
    Result := Self;
    Exit;
  end
  else
  if (Position > 1.0) then
  begin
    Result := Other;
    Exit;
  end;

  // calculate interpolation
  Result.FX := FX + Position * (Other.FX - FX);
  Result.FY := FY + Position * (Other.FY - FY);
  Result.FZ := FZ + Position * (Other.FZ - FZ);
end;
//------------------------------------------------------------------------------
{$ENDREGION}
//------------------------------------------------------------------------------
{$REGION 'TQRPlane'}
//------------------------------------------------------------------------------
constructor TQRPlane.Create(const A, B, C, D: Double);
begin
  FA := A;
  FB := B;
  FC := C;
  FD := D;
end;
//------------------------------------------------------------------------------
constructor TQRPlane.Create(const Other: TQRPlane);
begin
  Assign(Other);
end;
//------------------------------------------------------------------------------
procedure TQRPlane.Assign(const Other: TQRPlane);
begin
  FA := Other.FA;
  FB := Other.FB;
  FC := Other.FC;
  FD := Other.FD;
end;
//------------------------------------------------------------------------------
function TQRPlane.Invert: TQRPlane;
begin
  Result := TQRPlane.Create(-FA, -FB, -FC, -FD);
end;
//------------------------------------------------------------------------------
function TQRPlane.IsEqual(const Other: TQRPlane): Boolean;
begin
  Result := ((FA = Other.FA) and (FB = Other.FB) and (FC = Other.FC) and (FD = Other.FD));
end;
//------------------------------------------------------------------------------
function TQRPlane.Differs(const Other: TQRPlane): Boolean;
begin
  Result := ((FA <> Other.FA) or (FB <> Other.FB) or (FC <> Other.FC) or (FD <> Other.FD));
end;
//------------------------------------------------------------------------------
function TQRPlane.DistanceTo(const Point: TQRVector3D): Double;
var
  N: TQRVector3D;
begin
  // get the normal of the plane
  N := TQRVector3D.Create(FA, FB, FC);

  // calculate the distance between the plane and the point
  Result := N.Dot(Point) + FD;
end;
//------------------------------------------------------------------------------
function TQRPlane.IntersectLine(const V1, V2: TQRVector3D; out P: TQRVector3D): Boolean;
var
  Direction: TQRVector3D;
begin
  // calculates the direction of the line
  Direction := V2.Sub(V1);

  Result := IntersectRay(V1, Direction.Normalize, P);
end;
//------------------------------------------------------------------------------
function TQRPlane.IntersectRay(const Rp, Rd: TQRVector3D; out P: TQRVector3D): Boolean;
var
  Normal:    TQRVector3D;
  Dot, Temp: Double;
begin
  // gets the normal of the plane
  Normal := TQRVector3D.Create(FA, FB, FC);

  // calculates the angle between the line and the normal to the plane
  Dot := Normal.Dot(Rd);

  // if normal to the plane is perpendicular to the line, then the line is
  // either parallel to the plane and there are no solutions or the line is
  // on the plane in which case there are an infinite number of solutions
  if (Dot = 0.0) then
  begin
      Result := False;
      Exit;
  end;

  Temp := (FD + Normal.Dot(Rp)) / Dot;

  // calculates the intersection point
  P := TQRVector3D.Create(Rp.FX - (Temp * Rd.FX),
                          Rp.FY - (Temp * Rd.FY),
                          Rp.FZ - (Temp * Rd.FZ));

  Result := True;
end;
//------------------------------------------------------------------------------
function TQRPlane.Compare(const Other: TQRPlane; const Tolerance: Double): Boolean;
begin
  Result := (((FA >= (Other.FA - Tolerance))  and
              (FA <= (Other.FA + Tolerance))) and
             ((FB >= (Other.FB - Tolerance))  and
              (FB <= (Other.FB + Tolerance))) and
             ((FC >= (Other.FC - Tolerance))  and
              (FC <= (Other.FC + Tolerance))) and
             ((FD >= (Other.FD - Tolerance))  and
              (FD <= (Other.FD + Tolerance))));
end;
//------------------------------------------------------------------------------
class function TQRPlane.FromPoints(const V1, V2, V3: TQRVector3D): TQRPlane;
var
  E1, E2, Normal: TQRVector3D;
begin
  // calculate edge vectors
  E1 := V2.Sub(V1);
  E2 := V3.Sub(V1);

  // calculate the normal of the plane
  Normal := E1.Cross(E2).Normalize;

  // calculate and return the plane
  Result := FromPointNormal(V1, Normal);
end;
//------------------------------------------------------------------------------
class function TQRPlane.FromPointNormal(const Point, Normal: TQRVector3D): TQRPlane;
begin
  // the a, b, and c components are only the normal of the plane, and the D
  // component can be calculated using the aX + bY + cZ + d = 0 algorithm
  Result := TQRPlane.Create(Normal.FX, Normal.FY, Normal.FZ, -(Normal.Dot(Point)));
end;
//------------------------------------------------------------------------------
{$ENDREGION}
//------------------------------------------------------------------------------
{$REGION 'TQRMatrix4x4'}
//------------------------------------------------------------------------------
constructor TQRMatrix4x4.Create(const _11, _12, _13, _14,
                                      _21, _22, _23, _24,
                                      _31, _32, _33, _34,
                                      _41, _42, _43, _44: Double);
begin
  FTable[0][0] := _11; FTable[0][1] := _12; FTable[0][2] := _13; FTable[0][3] := _14;
  FTable[1][0] := _21; FTable[1][1] := _22; FTable[1][2] := _23; FTable[1][3] := _24;
  FTable[2][0] := _31; FTable[2][1] := _32; FTable[2][2] := _33; FTable[2][3] := _34;
  FTable[3][0] := _41; FTable[3][1] := _42; FTable[3][2] := _43; FTable[3][3] := _44;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRMatrix4x4.Create(const Other: TQRMatrix4x4);
begin
  Assign(Other);
end;
//--------------------------------------------------------------------------------------------------
class function TQRMatrix4x4.GetDefault: TQRMatrix4x4;
begin
  Result := Identity;
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.GetItem(Index: NativeInt): Double;
var
  X, Y: NativeInt;
begin
  if (Index > 15) then
    raise Exception.Create('Index is out of bounds');

  // calculate x and y position from index
  X := Index mod 4;
  Y := Index div 4;

  Result := FTable[X][Y];
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMatrix4x4.SetItem(Index: NativeInt; Value: Double);
var
  X, Y: NativeInt;
begin
  if (Index > 15) then
    raise Exception.Create('Index is out of bounds');

  // calculate x and y position from index
  X := Index mod 4;
  Y := Index div 4;

  FTable[X][Y] := Value;
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.GetTableItem(X, Y: NativeInt): Double;
begin
  if (X > 3) then
    raise Exception.Create('X index is out of bounds');

  if (Y > 3) then
    raise Exception.Create('Y index is out of bounds');

  Result := FTable[X][Y];
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMatrix4x4.SetTableItem(X, Y: NativeInt; Value: Double);
begin
  if (X > 3) then
    raise Exception.Create('X index is out of bounds');

  if (Y > 3) then
    raise Exception.Create('Y index is out of bounds');

  FTable[X][Y] := Value;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMatrix4x4.Assign(const Other: TQRMatrix4x4);
var
  I, J: Byte;
begin
  // copy matrix table from Other
  for I := 0 to 3 do
    for J := 0 to 3 do
      FTable[I][J] := Other.FTable[I][J];
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.IsEqual(const Other: TQRMatrix4x4): Boolean;
var
  I, J: Byte;
begin
  // compare each matrix element with Other matrix
  for I := 0 to 3 do
    for J := 0 to 3 do
      if (FTable[I][J] <> Other.FTable[I][J]) then
      begin
        Result := False;
        Exit;
      end;

  Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Differs(const Other: TQRMatrix4x4): Boolean;
begin
  Result := not IsEqual(Other);
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.IsIdentity: Boolean;
begin
  Result := IsEqual(Identity);
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Determinant: Double;
var
  T: array [0..2] of Double;
  V: array [0..3] of Double;
begin
  T[0] := FTable[2][2] * FTable[3][3] - FTable[2][3] * FTable[3][2];
  T[1] := FTable[1][2] * FTable[3][3] - FTable[1][3] * FTable[3][2];
  T[2] := FTable[1][2] * FTable[2][3] - FTable[1][3] * FTable[2][2];

  V[0] :=  FTable[1][1] * T[0] - FTable[2][1] * T[1] + FTable[3][1] * T[2];
  V[1] := -FTable[1][0] * T[0] + FTable[2][0] * T[1] - FTable[3][0] * T[2];

  T[0] := FTable[1][0] * FTable[2][1] - FTable[2][0] * FTable[1][1];
  T[1] := FTable[1][0] * FTable[3][1] - FTable[3][0] * FTable[1][1];
  T[2] := FTable[2][0] * FTable[3][1] - FTable[3][0] * FTable[2][1];

  V[2] :=  FTable[3][3] * T[0] - FTable[2][3] * T[1] + FTable[1][3] * T[2];
  V[3] := -FTable[3][2] * T[0] + FTable[2][2] * T[1] - FTable[1][2] * T[2];

  Result := FTable[0][0] * V[0] +
            FTable[0][1] * V[1] +
            FTable[0][2] * V[2] +
            FTable[0][3] * V[3];
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Inverse(out Determinant: Double): TQRMatrix4x4;
var
  InvDet: Double;
  T:      array [0..2]  of Double;
  V:      array [0..15] of Double;
  I, J:   NativeUInt;
begin
  T[0] := FTable[2][2] * FTable[3][3] - FTable[2][3] * FTable[3][2];
  T[1] := FTable[1][2] * FTable[3][3] - FTable[1][3] * FTable[3][2];
  T[2] := FTable[1][2] * FTable[2][3] - FTable[1][3] * FTable[2][2];

  V[0] :=  FTable[1][1] * T[0] - FTable[2][1] * T[1] + FTable[3][1] * T[2];
  V[4] := -FTable[1][0] * T[0] + FTable[2][0] * T[1] - FTable[3][0] * T[2];

  T[0] :=  FTable[1][0] * FTable[2][1] - FTable[2][0] * FTable[1][1];
  T[1] :=  FTable[1][0] * FTable[3][1] - FTable[3][0] * FTable[1][1];
  T[2] :=  FTable[2][0] * FTable[3][1] - FTable[3][0] * FTable[2][1];

  V[8]  :=  FTable[3][3] * T[0] - FTable[2][3] * T[1] + FTable[1][3] * T[2];
  V[12] := -FTable[3][2] * T[0] + FTable[2][2] * T[1] - FTable[1][2] * T[2];

  Determinant := FTable[0][0] * V[0] +
                 FTable[0][1] * V[4] +
                 FTable[0][2] * V[8] +
                 FTable[0][3] * V[12];

  if (Determinant = 0.0) then
    Exit;

  T[0] := FTable[2][2] * FTable[3][3] - FTable[2][3] * FTable[3][2];
  T[1] := FTable[0][2] * FTable[3][3] - FTable[0][3] * FTable[3][2];
  T[2] := FTable[0][2] * FTable[2][3] - FTable[0][3] * FTable[2][2];

  V[1] := -FTable[0][1] * T[0] + FTable[2][1] * T[1] - FTable[3][1] * T[2];
  V[5] :=  FTable[0][0] * T[0] - FTable[2][0] * T[1] + FTable[3][0] * T[2];

  T[0] := FTable[0][0] * FTable[2][1] - FTable[2][0] * FTable[0][1];
  T[1] := FTable[3][0] * FTable[0][1] - FTable[0][0] * FTable[3][1];
  T[2] := FTable[2][0] * FTable[3][1] - FTable[3][0] * FTable[2][1];

  V[9]  := -FTable[3][3] * T[0] - FTable[2][3] * T[1] - FTable[0][3] * T[2];
  V[13] :=  FTable[3][2] * T[0] + FTable[2][2] * T[1] + FTable[0][2] * T[2];

  T[0] := FTable[1][2] * FTable[3][3] - FTable[1][3] * FTable[3][2];
  T[1] := FTable[0][2] * FTable[3][3] - FTable[0][3] * FTable[3][2];
  T[2] := FTable[0][2] * FTable[1][3] - FTable[0][3] * FTable[1][2];

  V[2] :=  FTable[0][1] * T[0] - FTable[1][1] * T[1] + FTable[3][1] * T[2];
  V[6] := -FTable[0][0] * T[0] + FTable[1][0] * T[1] - FTable[3][0] * T[2];

  T[0] := FTable[0][0] * FTable[1][1] - FTable[1][0] * FTable[0][1];
  T[1] := FTable[3][0] * FTable[0][1] - FTable[0][0] * FTable[3][1];
  T[2] := FTable[1][0] * FTable[3][1] - FTable[3][0] * FTable[1][1];

  V[10] :=  FTable[3][3] * T[0] + FTable[1][3] * T[1] + FTable[0][3] * T[2];
  V[14] := -FTable[3][2] * T[0] - FTable[1][2] * T[1] - FTable[0][2] * T[2];

  T[0] := FTable[1][2] * FTable[2][3] - FTable[1][3] * FTable[2][2];
  T[1] := FTable[0][2] * FTable[2][3] - FTable[0][3] * FTable[2][2];
  T[2] := FTable[0][2] * FTable[1][3] - FTable[0][3] * FTable[1][2];

  V[3] := -FTable[0][1] * T[0] + FTable[1][1] * T[1] - FTable[2][1] * T[2];
  V[7] :=  FTable[0][0] * T[0] - FTable[1][0] * T[1] + FTable[2][0] * T[2];

  V[11] := -FTable[0][0] * (FTable[1][1] * FTable[2][3] - FTable[1][3] * FTable[2][1]) +
            FTable[1][0] * (FTable[0][1] * FTable[2][3] - FTable[0][3] * FTable[2][1]) -
            FTable[2][0] * (FTable[0][1] * FTable[1][3] - FTable[0][3] * FTable[1][1]);

  V[15] := FTable[0][0] * (FTable[1][1] * FTable[2][2] - FTable[1][2] * FTable[2][1]) -
           FTable[1][0] * (FTable[0][1] * FTable[2][2] - FTable[0][2] * FTable[2][1]) +
           FTable[2][0] * (FTable[0][1] * FTable[1][2] - FTable[0][2] * FTable[1][1]);

  InvDet := 1.0 / Determinant;

  for I := 0 to 3 do
    for J := 0 to 3 do
      Result.FTable[I][J] := V[4 * I + J] * InvDet;
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Multiply(const Other: TQRMatrix4x4): TQRMatrix4x4;
var
  I, J: Byte;
begin
    for i := 0 to 3 do
        for j := 0 to 3 do
            Result.FTable[i][j] := FTable[i][0] * Other.FTable[0][j] +
                                    FTable[i][1] * Other.FTable[1][j] +
                                    FTable[i][2] * Other.FTable[2][j] +
                                    FTable[i][3] * Other.FTable[3][j];
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Translate(const t: TQRVector3D): TQRMatrix4x4;
begin
    FTable[3][0] := FTable[3][0] + (FTable[0][0] * t.FX + FTable[1][0] * t.FY + FTable[2][0] * t.FZ);
    FTable[3][1] := FTable[3][1] + (FTable[0][1] * t.FX + FTable[1][1] * t.FY + FTable[2][1] * t.FZ);
    FTable[3][2] := FTable[3][2] + (FTable[0][2] * t.FX + FTable[1][2] * t.FY + FTable[2][2] * t.FZ);
    FTable[3][3] := FTable[3][3] + (FTable[0][3] * t.FX + FTable[1][3] * t.FY + FTable[2][3] * t.FZ);

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Rotate(const angle: Double; const r: TQRVector3D): TQRMatrix4x4;
var
    c, s, ic: Double;
    matrix:   TQRMatrix4x4;
begin
    // calculate sinus, cosinus and inverted cosinus Values
    c  := cos(angle);
    s  := sin(angle);
    ic := (1.0 - c);

    // create rotation matrix
    matrix               := Identity;
    matrix.FTable[0][0] := (ic * r.FX * r.FX) + c;
    matrix.FTable[1][0] := (ic * r.FX * r.FY) - (s * r.FZ);
    matrix.FTable[2][0] := (ic * r.FX * r.FZ) + (s * r.FY);
    matrix.FTable[0][1] := (ic * r.FY * r.FX) + (s * r.FZ);
    matrix.FTable[1][1] := (ic * r.FY * r.FY) + c;
    matrix.FTable[2][1] := (ic * r.FY * r.FZ) - (s * r.FX);
    matrix.FTable[0][2] := (ic * r.FZ * r.FX) - (s * r.FY);
    matrix.FTable[1][2] := (ic * r.FZ * r.FY) + (s * r.FX);
    matrix.FTable[2][2] := (ic * r.FZ * r.FZ) + c;

    // combine current matrix with rotation matrix
    Self := matrix.Multiply(Self);

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Scale(const s: TQRVector3D): TQRMatrix4x4;
begin
    FTable[0][0] := FTable[0][0] * s.FX; FTable[1][0] := FTable[1][0] * s.FY; FTable[2][0] := FTable[2][0] * s.FZ;
    FTable[0][1] := FTable[0][1] * s.FX; FTable[1][1] := FTable[1][1] * s.FY; FTable[2][1] := FTable[2][1] * s.FZ;
    FTable[0][2] := FTable[0][2] * s.FX; FTable[1][2] := FTable[1][2] * s.FY; FTable[2][2] := FTable[2][2] * s.FZ;
    FTable[0][3] := FTable[0][3] * s.FX; FTable[1][3] := FTable[1][3] * s.FY; FTable[2][3] := FTable[2][3] * s.FZ;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Swap: TQRMatrix4x4;
begin
    Result := TQRMatrix4x4.Create(FTable[0][0], FTable[1][0], FTable[2][0], FTable[3][0],
                                  FTable[0][1], FTable[1][1], FTable[2][1], FTable[3][1],
                                  FTable[0][2], FTable[1][2], FTable[2][2], FTable[3][2],
                                  FTable[0][3], FTable[1][3], FTable[2][3], FTable[3][3]);
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Transform(const vector: TQRVector3D): TQRVector3D;
begin
    // calculates x, y and z coordinates (don't use w component), and returns
    // transformed vector
    Result := TQRVector3D.Create((vector.FX * FTable[0][0] +
                                  vector.FY * FTable[1][0] +
                                  vector.FZ * FTable[2][0] +
                                  FTable[3][0]),
                                 (vector.FX * FTable[0][1] +
                                  vector.FY * FTable[1][1] +
                                  vector.FZ * FTable[2][1] +
                                  FTable[3][1]),
                                 (vector.FX * FTable[0][2] +
                                  vector.FY * FTable[1][2] +
                                  vector.FZ * FTable[2][2] +
                                  FTable[3][2]));
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.GetPtr: PDouble;
begin
    Result := @FTable[0][0];
end;
//--------------------------------------------------------------------------------------------------
class function TQRMatrix4x4.Identity: TQRMatrix4x4;
begin
    Result := TQRMatrix4x4.Create(1.0, 0.0, 0.0, 0.0,
                                  0.0, 1.0, 0.0, 0.0,
                                  0.0, 0.0, 1.0, 0.0,
                                  0.0, 0.0, 0.0, 1.0);
end;
//--------------------------------------------------------------------------------------------------
// TQRQuaternion
//--------------------------------------------------------------------------------------------------
constructor TQRQuaternion.Create(x, y, z, w: Double);
begin
    FX := x;
    FY := y;
    FZ := z;
    m_W := w;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRQuaternion.Create(const vector: TQRVector3D; angle: Double);
var
    sinAngle: Double;
begin
    sinAngle := Sin(angle);

    FX := (vector.FX * sinAngle);
    FY := (vector.FY * sinAngle);
    FZ := (vector.FZ * sinAngle);
    m_W :=  Cos(angle);
end;
//--------------------------------------------------------------------------------------------------
constructor TQRQuaternion.Create(const matrix: TQRMatrix4x4);
var
    diagonal, scale: Double;
begin
    // calculate the matrix diagonal by adding up it's diagonal indices (also known as "trace")
    diagonal := matrix.FTable[0][0] +
                matrix.FTable[1][1] +
                matrix.FTable[2][2] +
                matrix.FTable[3][3];

    // is diagonal greater than zero?
    if (diagonal > 0.00000001) then
    begin
        // calculate the diagonal scale
        scale := Sqrt(diagonal) * 2.0;

        // calculate the quaternion Values using the respective equation
        FX := (matrix.FTable[1][2] - matrix.FTable[2][1]) / scale;
        FY := (matrix.FTable[2][0] - matrix.FTable[0][2]) / scale;
        FZ := (matrix.FTable[0][1] - matrix.FTable[1][0]) / scale;
        m_W := 0.25 * scale;

        Exit;
    end;

    // search for highest Value in the matrix diagonal
    if ((matrix.FTable[0][0] > matrix.FTable[1][1]) and (matrix.FTable[0][0] > matrix.FTable[2][2])) then
    begin
        // calculate scale using the first diagonal element and double that Value
        scale := Sqrt(1.0 + matrix.FTable[0][0] - matrix.FTable[1][1] - matrix.FTable[2][2]) * 2.0;

        // calculate the quaternion Values using the respective equation
        FX := 0.25 * scale;
        FY := (matrix.FTable[0][1] + matrix.FTable[1][0]) / scale;
        FZ := (matrix.FTable[2][0] + matrix.FTable[0][2]) / scale;
        m_W := (matrix.FTable[1][2] - matrix.FTable[2][1]) / scale;
    end
    else
    if (matrix.FTable[1][1] > matrix.FTable[2][2]) then
    begin
        // calculate scale using the second diagonal element and double that Value
        scale := Sqrt(1.0 + matrix.FTable[1][1] - matrix.FTable[0][0] - matrix.FTable[2][2]) * 2.0;

        // calculate the quaternion Values using the respective equation
        FX := (matrix.FTable[0][1] + matrix.FTable[1][0]) / scale;
        FY := 0.25 * scale;
        FZ := (matrix.FTable[1][2] + matrix.FTable[2][1]) / scale;
        m_W := (matrix.FTable[2][0] - matrix.FTable[0][2]) / scale;
    end
    else
    begin
        // calculate scale using the third diagonal element and double that Value
        scale := Sqrt(1.0 + matrix.FTable[2][2] - matrix.FTable[0][0] - matrix.FTable[1][1]) * 2.0;

        // calculate the quaternion Values using the respective equation
        FX := (matrix.FTable[2][0] + matrix.FTable[0][2]) / scale;
        FY := (matrix.FTable[1][2] + matrix.FTable[2][1]) / scale;
        FZ := 0.25 * scale;
        m_W := (matrix.FTable[0][1] - matrix.FTable[1][0]) / scale;
    end
end;
//--------------------------------------------------------------------------------------------------
constructor TQRQuaternion.Create(const Other: TQRQuaternion);
begin
    Assign(Other);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRQuaternion.Assign(const Other: TQRQuaternion);
begin
    FX := Other.FX;
    FY := Other.FY;
    FZ := Other.FZ;
    m_W := Other.m_W;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Add(const Value: Double): TQRQuaternion;
begin
    Result := TQRQuaternion.Create(FX + Value, FY + Value, FZ + Value, m_W + Value);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Add(const Other: TQRQuaternion): TQRQuaternion;
begin
    Result := TQRQuaternion.Create(FX + Other.FX,
                                   FY + Other.FY,
                                   FZ + Other.FZ,
                                   m_W + Other.m_W);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Invert: TQRQuaternion;
begin
    Result := TQRQuaternion.Create(-FX, -FY, -FZ, -m_W);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Sub(const Value: Double): TQRQuaternion;
begin
    Result := TQRQuaternion.Create(FX - Value, FY - Value, FZ - Value, m_W - Value);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Sub(const Other: TQRQuaternion): TQRQuaternion;
begin
    Result := TQRQuaternion.Create(FX - Other.FX,
                                   FY - Other.FY,
                                   FZ - Other.FZ,
                                   m_W - Other.m_W);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Mul(const Value: Double): TQRQuaternion;
begin
    Result := TQRQuaternion.Create(FX * Value, FY * Value, FZ * Value, m_W * Value);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Mul(const Other: TQRQuaternion): TQRQuaternion;
begin
    Result := TQRQuaternion.Create(FX * Other.FX,
                                   FY * Other.FY,
                                   FZ * Other.FZ,
                                   m_W * Other.m_W);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Divide(const Value: Double): TQRQuaternion;
begin
    if (Value = 0.0) then
        raise Exception.Create('Division by 0 is prohibited');

    Result := TQRQuaternion.Create(FX / Value, FY / Value, FZ / Value, m_W / Value);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Divide(const Other: TQRQuaternion): TQRQuaternion;
begin
    if (Other.FX = 0.0) then
        raise Exception.Create('Quaternion x Value - division by 0 is prohibited');

    if (Other.FY = 0.0) then
        raise Exception.Create('Quaternion y Value - division by 0 is prohibited');

    if (Other.FZ = 0.0) then
        raise Exception.Create('Quaternion z Value - division by 0 is prohibited');

    if (Other.m_W = 0.0) then
        raise Exception.Create('Quaternion z Value - division by 0 is prohibited');

    Result := TQRQuaternion.Create(FX / Other.FX,
                                   FY / Other.FY,
                                   FZ / Other.FZ,
                                   m_W / Other.m_W);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.AddAndAssign(const Value: Double): TQRQuaternion;
begin
    FX := FX + Value;
    FY := FY + Value;
    FZ := FZ + Value;
    m_W := m_W + Value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.AddAndAssign(const Other: TQRQuaternion): TQRQuaternion;
begin
    FX := FX + Other.FX;
    FY := FY + Other.FY;
    FZ := FZ + Other.FZ;
    m_W := m_W + Other.m_W;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.SubAndAssign(const Value: Double): TQRQuaternion;
begin
    FX := FX - Value;
    FY := FY - Value;
    FZ := FZ - Value;
    m_W := m_W - Value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.SubAndAssign(const Other: TQRQuaternion): TQRQuaternion;
begin
    FX := FX - Other.FX;
    FY := FY - Other.FY;
    FZ := FZ - Other.FZ;
    m_W := m_W - Other.m_W;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.MulAndAssign(const Value: Double): TQRQuaternion;
begin
    FX := FX * Value;
    FY := FY * Value;
    FZ := FZ * Value;
    m_W := m_W * Value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.MulAndAssign(const Other: TQRQuaternion): TQRQuaternion;
begin
    FX := FX * Other.FX;
    FY := FY * Other.FY;
    FZ := FZ * Other.FZ;
    m_W := m_W * Other.m_W;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.DivAndAssign(const Value: Double): TQRQuaternion;
begin
    if (Value = 0.0) then
        raise Exception.Create('Division by 0 is prohibited');

    FX := FX / Value;
    FY := FY / Value;
    FZ := FZ / Value;
    m_W := m_W / Value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.DivAndAssign(const Other: TQRQuaternion): TQRQuaternion;
begin
    if (Other.FX = 0.0) then
        raise Exception.Create('Quaternion x Value - division by 0 is prohibited');

    if (Other.FY = 0.0) then
        raise Exception.Create('Quaternion y Value - division by 0 is prohibited');

    if (Other.FZ = 0.0) then
        raise Exception.Create('Quaternion z Value - division by 0 is prohibited');

    if (Other.m_W = 0.0) then
        raise Exception.Create('Quaternion w Value - division by 0 is prohibited');

    FX := FX / Other.FX;
    FY := FY / Other.FY;
    FZ := FZ / Other.FZ;
    m_W := m_W / Other.m_W;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.IsEqual(const Other: TQRQuaternion): Boolean;
begin
    Result := ((FX = Other.FX) and (FY = Other.FY) and (FZ = Other.FZ) and (m_W = Other.m_W));
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Differs(const Other: TQRQuaternion): Boolean;
begin
    Result := ((FX <> Other.FX) or (FY <> Other.FY) or (FZ <> Other.FZ) or (m_W <> Other.m_W));
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Norm: Double;
begin
    Result := ((FX * FX) + (FY * FY) + (FZ * FZ) + (m_W * m_W));
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Length: Double;
begin
    Result := Sqrt(Norm);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Normalize: TQRQuaternion;
var
    len: Double;
begin
    len := Length;

    if (len = 0.0) then
    begin
        Result := Default(TQRQuaternion);
        Exit;
    end;

    Result := TQRQuaternion.Create((FX / len), (FY / len), (FZ / len), (m_W / len));
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Dot(const q: TQRQuaternion): Double;
begin
    Result := ((FX * q.FX) + (FY * q.FY) + (FZ * q.FZ) + (m_W * q.m_W));
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Scale(s: Double): TQRQuaternion;
begin
    Result := TQRQuaternion.Create(FX * s, FY * s, FZ * s, m_W * s);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Conjugate: TQRQuaternion;
begin
    Result := TQRQuaternion.Create(-FX, -FY, -FZ, m_W);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Inverse: TQRQuaternion;
var
    quatNorm: Double;
begin
    // calculate the norm of the quaternion
    quatNorm := Norm;

    // empty quaternion?
    if (quatNorm = 0.0) then
    begin
        Result := Default(TQRQuaternion);
        Exit;
    end;

    Result := Conjugate.Scale(1.0 / quatNorm);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Slerp(const Other: TQRQuaternion; p: Double): TQRQuaternion;
var
    quatDot, scale0, scale1, theta, sinTheta: Double;
    interpolateWith:                          TQRQuaternion;
begin
    // are quaternions identical?
    if (IsEqual(Other)) then
    begin
        Result := Self;
        Exit;
    end;

    // calculate dot product between q1 and q2
    quatDot := Dot(Other);

    // check if angle is higher than 90° (this happen if dot product is less than 0)
    if (quatDot < 0.0) then
    begin
        // negate the second quaternion and the dot product result
        interpolateWith :=  Other.Invert;
        quatDot         := -quatDot;
    end
    else
        interpolateWith := Other;

    // calculate the interpolation first and second scale
    scale0 := 1.0 - p;
    scale1 := p;

    // is angle large enough to apply the calculation
    if ((1.0 - quatDot) > 0.1) then
    begin
        // calculate the angle between the 2 quaternions and get the sinus of that angle
        theta    := ArcCos(quatDot);
        sinTheta := Sin(theta);

        // is resulting sinus equal to 0? (just to verify, should not happen)
        if (sinTheta = 0.0) then
            raise Exception.Create('Invalid Value');

        // calculate the scaling for q1 and q2, according to the angle and it's sine Value
        scale0 := Sin((1.0 - p) * theta)  / sinTheta;
        scale1 := Sin((p        * theta)) / sinTheta;
    end;

    // calculate the resulting quaternion by using a special form of linear interpolation
    Result.FX := (scale0 * FX) + (scale1 * interpolateWith.FX);
    Result.FY := (scale0 * FY) + (scale1 * interpolateWith.FY);
    Result.FZ := (scale0 * FZ) + (scale1 * interpolateWith.FZ);
    Result.m_W := (scale0 * m_W) + (scale1 * interpolateWith.m_W);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Rotate(const vector: TQRVector3D): TQRVector3D;
var
    qv, qm: TQRQuaternion;
begin
    // rotate vector
    qv := TQRQuaternion.Create(vector.FX, vector.FY, vector.FZ, 0);
    qm := Mul(qv.Mul(Inverse));

    Result.FX := qm.FX;
    Result.FY := qm.FY;
    Result.FZ := qm.FZ;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.GetMatrix: TQRMatrix4x4;
begin
    Result := TQRMAtrix4x4.Create(1.0 -  2.0 * (FY * FY + FZ * FZ), 2.0 * (FX *  FY - m_W * FZ),       2.0 * (FX *  FZ + m_W * FY),       0.0,
                                  2.0 * (FX *  FY + m_W * FZ),       1.0 -  2.0 * (FX * FX + FZ * FZ), 2.0 * (FY *  FZ - m_W * FX),       0.0,
                                  2.0 * (FX *  FZ - m_W * FY),       2.0 * (FY *  FZ + m_W * FX),       1.0 -  2.0 * (FX * FX + FY * FY), 0.0,
                                  0.0,                                  0.0,                                  0.0,                                  1.0);
end;
//--------------------------------------------------------------------------------------------------
// TQRRay
//--------------------------------------------------------------------------------------------------
constructor TQRRay.Create;
var
    dir: TQRVector3D;
begin
    FPos := Default(TQRVector3D);
    dir   := Default(TQRVector3D);
    SetDir(@dir);
end;
//--------------------------------------------------------------------------------------------------
constructor TQRRay.Create(const pOther: TQRRay);
begin
    FPos.Assign(pOther.FPos);
    FDir.Assign(pOther.FDir);
    m_InvDir.Assign(pOther.m_InvDir);
end;
//--------------------------------------------------------------------------------------------------
function TQRRay.GetPos: PQRVector3D;
begin
    Result := @FPos;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRRay.SetPos(const pPos: PQRVector3D);
begin
    FPos := pPos^;
end;
//--------------------------------------------------------------------------------------------------
function TQRRay.GetDir: PQRVector3D;
begin
    Result := @FDir;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRRay.SetDir(const pDir: PQRVector3D);
begin
    FDir.Assign(pDir^);

    // is x direction empty?
    if (FDir.FX = 0.0) then
        // in this case invert direction on x axis is infinite
        m_InvDir.FX := Infinity
    else
        // calculate invert direction on x axis
        m_InvDir.FX := (1.0 / FDir.FX);

    // is y direction empty?
    if (FDir.FY = 0.0) then
        // in this case invert direction on y axis is infinite
        m_InvDir.FY := Infinity
    else
        // calculate invert direction on y axis
        m_InvDir.FY := (1.0 / FDir.FY);

    // is z direction empty?
    if (FDir.FZ = 0.0) then
        // in this case invert direction on z axis is infinite
        m_InvDir.FZ := Infinity
    else
        // calculate invert direction on z axis
        m_InvDir.FZ := (1.0 / FDir.FZ);
end;
//--------------------------------------------------------------------------------------------------
function TQRRay.GetInvDir: PQRVector3D;
begin
    Result := @m_InvDir;
end;
//--------------------------------------------------------------------------------------------------
// TQRPolygon
//--------------------------------------------------------------------------------------------------
function TQRPolygon.IsBetween(const Value, vStart, vEnd: TQRVector3D; tolerance: Double): Boolean;
begin
    // check if each vector component is between start and end limits
    Result := (IsBetween(Value.X, vStart.X, vEnd.X, tolerance) and
               IsBetween(Value.Y, vStart.Y, vEnd.Y, tolerance) and
               IsBetween(Value.Z, vStart.Z, vEnd.Z, tolerance));
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.IsBetween(const Value, lStart, lEnd, tolerance: Double): Boolean;
begin
    // check if each Value is between start and end limits considering tolerance
    Result := ((Value >= Min(lStart, lEnd) - tolerance) and
               (Value <= Max(lStart, lEnd) + tolerance));
end;
//--------------------------------------------------------------------------------------------------
constructor TQRPolygon.Create(const vertex1, vertex2, vertex3: TQRVector3D);
begin
    m_Vertex[0] := vertex1;
    m_Vertex[1] := vertex2;
    m_Vertex[2] := vertex3;
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.GetVertex(index: Byte): TQRVector3D;
begin
    // search for index to get
    case (index) of
        0:
            Result := m_Vertex[0];

        1:
            Result := m_Vertex[1];

        2:
            Result := m_Vertex[2];
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPolygon.SetVertex(index: Byte; const vertex: TQRVector3D);
begin
    // search for index to set
    case (index) of
        0:
            m_Vertex[0] := vertex;

        1:
            m_Vertex[1] := vertex;

        2:
            m_Vertex[2] := vertex;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.GetVertex1: PQRVector3D;
begin
    Result := @m_Vertex[0];
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPolygon.SetVertex1(const pVertex: PQRVector3D);
begin
    m_Vertex[0] := pVertex^;
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.GetVertex2: PQRVector3D;
begin
    Result := @m_Vertex[1];
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPolygon.SetVertex2(const pVertex: PQRVector3D);
begin
    m_Vertex[1] := pVertex^;
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.GetVertex3: PQRVector3D;
begin
    Result := @m_Vertex[2];
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPolygon.SetVertex3(const pVertex: PQRVector3D);
begin
    m_Vertex[2] := pVertex^;
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.GetClone: TQRPolygon;
begin
    // copies the polygon, then returns the copy
    Result := TQRPolygon.Create(m_Vertex[0], m_Vertex[1], m_Vertex[2]);
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.ApplyMatrix(const matrix: TQRMatrix4x4): TQRPolygon;
begin
    // build a new polygon transforming all vertices of the polygon using
    // given matrix, and return new built polygon
    Result := TQRPolygon.Create(matrix.Transform(m_Vertex[0]),
                                matrix.Transform(m_Vertex[1]),
                                matrix.Transform(m_Vertex[2]));
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.GetPlane: TQRPlane;
begin
    // calculates the plane from the Values of the 3 vertices of the polygon
    Result := TQRPlane.FromPoints(m_Vertex[0], m_Vertex[1], m_Vertex[2]);
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.GetCenter: TQRVector3D;
begin
    // calculates then returns the Value of the midpoint of the polygon
    Result := TQRVector3D.Create(((m_Vertex[0].X + m_Vertex[1].X + m_Vertex[2].X) / 3.0),
                                 ((m_Vertex[0].Y + m_Vertex[1].Y + m_Vertex[2].Y) / 3.0),
                                 ((m_Vertex[0].Z + m_Vertex[1].Z + m_Vertex[2].Z) / 3.0));
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.Inside(const x, y, z: Double): Boolean;
begin
    Result := Inside(TQRVector3D.Create(x, y, z));
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.Inside(const point: TQRVector3D): Boolean;
var
    nPToV1, nPToV2, nPToV3:  TQRVector3D;
    a1, a2, a3, angleResult: Double;
begin
    {*
    * check if the point p is inside the polygon in the following manner:
    *
    *                  V1                         V1
    *                  /\                         /\
    *                 /  \                       /  \
    *                / *p \                  *P /    \
    *               /      \                   /      \
    *            V2 -------- V3             V2 -------- V3
    *
    * calculate the vectors between the point p and each polygon vertex, then
    * calculate the angle formed by each of these vectors. If the sum of the
    * angles is equal to a complete circle, i.e. 2 * pi in radians, then the
    * point p is inside the polygon limits, otherwise the point is outside. It
    * is assumed that the point to check belongs to the polygon's plane
    *}
    nPToV1 := m_Vertex[0].Sub(point).Normalize;
    nPToV2 := m_Vertex[1].Sub(point).Normalize;
    nPToV3 := m_Vertex[2].Sub(point).Normalize;

    // calculate the angles using the dot product of each vectors. Limit range
    // to Values between -1.0f and 1.0f
    a1 := Max(Min(nPToV1.Dot(nPToV2), 1.0), -1.0);
    a2 := Max(Min(nPToV2.Dot(nPToV3), 1.0), -1.0);
    a3 := Max(Min(nPToV3.Dot(nPToV1), 1.0), -1.0);

    // calculate the sum of all angles
    angleResult := ArcCos(a1) + ArcCos(a2) + ArcCos(a3);

    // if sum is equal to 6.28 radians then point p is inside polygon. NOTE can
    // be higher due to precision errors in calculations
    Result := (angleResult >= 6.28);
end;
//--------------------------------------------------------------------------------------------------
// TQRCircle
//--------------------------------------------------------------------------------------------------
function TQRCircle.GetPos: PQRVector2D;
begin
    Result := @FPos;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRCircle.SetPos(const pPos: PQRVector2D);
begin
    FPos := pPos^;
end;
//--------------------------------------------------------------------------------------------------
// TQRSphere
//--------------------------------------------------------------------------------------------------
function TQRSphere.GetPos: PQRVector3D;
begin
    Result := @FPos;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRSphere.SetPos(const pPos: PQRVector3D);
begin
    FPos := pPos^;
end;
//--------------------------------------------------------------------------------------------------
// TQRRect
//--------------------------------------------------------------------------------------------------
constructor TQRRect.Create(const x, y, width, height: Double);
begin
    FMin := TQRVector2D.Create(x,         y);
    FMax := TQRVector2D.Create(x + width, y + height);
end;
//--------------------------------------------------------------------------------------------------
function TQRRect.GetMin: PQRVector2D;
begin
    Result := @FMin;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRRect.SetMin(const pValue: PQRVector2D);
begin
    FMin := pValue^;
end;
//--------------------------------------------------------------------------------------------------
function TQRRect.GetMax: PQRVector2D;
begin
    Result := @FMax;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRRect.SetMax(const pValue: PQRVector2D);
begin
    FMax := pValue^;
end;
//--------------------------------------------------------------------------------------------------
function TQRRect.GetWidth: Double;
begin
    Result := (FMax.FX - FMin.FX);
end;
//--------------------------------------------------------------------------------------------------
function TQRRect.GetHeight: Double;
begin
    Result := (FMax.FY - FMin.FY);
end;
//--------------------------------------------------------------------------------------------------
// TQRBox
//--------------------------------------------------------------------------------------------------
function TQRBox.GetMin: PQRVector3D;
begin
    Result := @FMin;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRBox.SetMin(const pValue: PQRVector3D);
begin
    FMin := pValue^;
end;
//--------------------------------------------------------------------------------------------------
function TQRBox.GetMax: PQRVector3D;
begin
    Result := @FMax;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRBox.SetMax(const pValue: PQRVector3D);
begin
    FMax := pValue^;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRBox.Cut(var leftBox: TQRBox; var rightBox: TQRBox);
var
    x, y, z:     Double;
    longestAxis: NativeUInt;
begin
    // calculate each edge length
    x := Abs(FMax.FX - FMin.FX);
    y := Abs(FMax.FY - FMin.FY);
    z := Abs(FMax.FZ - FMin.FZ);

    // search for longest axis
    if ((x >= y) and (x >= z)) then
        longestAxis := 0
    else
    if ((y >= x) and (y >= z)) then
        longestAxis := 1
    else
        longestAxis := 2;

    // cut box
    case longestAxis of
        // cut on x axis
        0:
        begin
            leftBox.FMin.Assign(FMin);
            leftBox.FMax.Assign(FMax);
            leftBox.FMax.FX := FMin.FX + (x * 0.5);

            rightBox.FMin.Assign(FMin);
            rightBox.FMax.Assign(FMax);
            rightBox.FMin.FX := leftBox.FMax.FX;
        end;

        // cut on y axis
        1:
        begin
            leftBox.FMin.Assign(FMin);
            leftBox.FMax.Assign(FMax);
            leftBox.FMax.FY := FMin.FY + (y * 0.5);

            rightBox.FMin.Assign(FMin);
            rightBox.FMax.Assign(FMax);
            rightBox.FMin.FY := leftBox.FMax.FY;
        end;

        // cut on z axis
        2:
        begin
            leftBox.FMin.Assign(FMin);
            leftBox.FMax.Assign(FMax);
            leftBox.FMax.FZ := FMin.FZ + (z * 0.5);

            rightBox.FMin.Assign(FMin);
            rightBox.FMax.Assign(FMax);
            rightBox.FMin.FZ := leftBox.FMax.FZ;
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------

end.
