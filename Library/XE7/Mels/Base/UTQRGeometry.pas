{******************************************************************************}
{ ==> UTQRGeometry ------------------------------------------------------------}
{******************************************************************************}
{ Description: Provides base classes for geometry, as e.g. vectors, ...        }
{ Developer:   Jean-Milost Reymond                                             }
{ License:     MIT License                                                     }
{ Copyright:   (c) 2015 - 2016, this file is part of the Mels library          }
{******************************************************************************}
{ MIT License                                                                  }
{                                                                              }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"), to     }
{ deal in the Software without restriction, including without limitation the   }
{ rights to use, copy, modify, merge, publish, distribute, sublicense, and/or  }
{ sell copies of the Software, and to permit persons to whom the Software is   }
{ furnished to do so, subject to the following conditions:                     }
{                                                                              }
{ The above copyright notice and this permission notice shall be included in   }
{ all copies or substantial portions of the Software.                          }
{                                                                              }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      }
{ FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER          }
{ DEALINGS IN THE SOFTWARE.                                                    }
{******************************************************************************}

unit UTQRGeometry;

interface

uses System.SysUtils,
     System.Math;

type

  {$REGION 'Documentation'}
  /// <summary>Vector 2D</summary>
  {$ENDREGION}
  TQRVector2D = record
  private
    FX: Single; // x coordinate for the 2D vector
    FY: Single; // y coordinate for the 2D vector

  public
    {$REGION 'Documentation'}
    /// <summary>Constructor</summary>
    /// <param name="X">Vector x coordinate</summary>
    /// <param name="Y">Vector y coordinate</summary>
    {$ENDREGION}
    constructor Create(const X, Y: Single); overload;

    {$REGION 'Documentation'}
    /// <summary>Copy constructor</summary>
    /// <param name="Other">Other vector to copy from</summary>
    {$ENDREGION}
    constructor Create(const Other: TQRVector2D); overload;

    {$REGION 'Documentation'}
    /// <summary>Assigns (i.e. copies) the content from another vector</summary>
    /// <param name="Other">Other vector to copy from</param>
    {$ENDREGION}
    procedure Assign(const Other: TQRVector2D); inline;

    {$REGION 'Documentation'}
    /// <summary>Compares the content of 2 vectors and determines if they are equal</summary>
    /// <param name="Other">Other vector to compare with</param>
    /// <returns>True if vectors are equal, otherwise False</returns>
    {$ENDREGION}
    function IsEqual(const Other: TQRVector2D): Boolean; inline;

    {$REGION 'Documentation'}
    /// <summary>Compares the content of 2 vectors and determines if they are different</summary>
    /// <param name="Other">Other vector to compare with</param>
    /// <returns>True if vectors differ, otherwise False</returns>
    {$ENDREGION}
    function Differs(const Other: TQRVector2D): Boolean; inline;

    {$REGION 'Documentation'}
    /// <summary>Inverts the vector</summary>
    /// <returns>Inverted vector</returns>
    {$ENDREGION}
    function Invert: TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Adds a value to this vector</summary>
    /// <param name="Value">Value to add to vector</param>
    /// <returns>Added vector</returns>
    {$ENDREGION}
    function Add(const Value: Single): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Adds the content of another vector to this vector</summary>
    /// <param name="Other">Other vector to add to this vector</param>
    /// <returns>Added vector</returns>
    {$ENDREGION}
    function Add(const Other: TQRVector2D): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Subtracts a value to this vector</summary>
    /// <param name="Value">Value to subtract to this vector</param>
    /// <returns>Subtracted vector</returns>
    {$ENDREGION}
    function Sub(const Value: Single): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Subtracts to this vector the contents of another vector</summary>
    /// <param name="Other">Other vector to subtract to this vector</param>
    /// <returns>Subtracted vector</returns>
    {$ENDREGION}
    function Sub(const Other: TQRVector2D): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Multiplies a value to this vector</summary>
    /// <param name="Value">Value to multiply to this vector</param>
    /// <returns>Multiplied vector</returns>
    {$ENDREGION}
    function Mul(const Value: Single): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Multiplies the content of another vector to this vector</summary>
    /// <param name="Other">Other vector to multiply to this vector</param>
    /// <returns>Multiplied vector</returns>
    {$ENDREGION}
    function Mul(const Other: TQRVector2D): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Divides a value to this vector</summary>
    /// <param name="Value">Value to divide to this vector</param>
    /// <returns>Divided vector</returns>
    {$ENDREGION}
    function Divide(const Value: Single): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Divides the content of another vector to this vector</summary>
    /// <param name="Other">Other vector to divide to this vector</param>
    /// <returns>Divided vector</returns>
    {$ENDREGION}
    function Divide(const Other: TQRVector2D): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Adds a value and assigns the result to this vector</summary>
    /// <param name="Value">Value to add to vector</param>
    /// <returns>Added vector</returns>
    {$ENDREGION}
    function AddAndAssign(const Value: Single): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Adds the content of another vector and assigns the result to this vector</summary>
    /// <param name="Other">Other vector to add to this vector</param>
    /// <returns>Added vector</returns>
    {$ENDREGION}
    function AddAndAssign(const Other: TQRVector2D): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Subtracts a value and assigns the result to this vector</summary>
    /// <param name="Value">Value to subtract to vector</param>
    /// <returns>Subtracted vector</returns>
    {$ENDREGION}
    function SubAndAssign(const Value: Single): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Subtracts the content of another vector and assigns the result to this vector</summary>
    /// <param name="Other">Other vector to subtract to this vector</param>
    /// <returns>Subtracted vector</returns>
    {$ENDREGION}
    function SubAndAssign(const Other: TQRVector2D): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Multilpies a value and assigns the result to this vector</summary>
    /// <param name="Value">Value to multiply to vector</param>
    /// <returns>Multiplied vector</returns>
    {$ENDREGION}
    function MulAndAssign(const Value: Single): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Multiplies the content of another vector and assigns the result to this vector</summary>
    /// <param name="Other">Other vector to multiply to this vector</param>
    /// <returns>Multiplied vector</returns>
    {$ENDREGION}
    function MulAndAssign(const Other: TQRVector2D): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Divides a value and assigns the result to this vector</summary>
    /// <param name="Value">Value to divide to vector</param>
    /// <returns>Divided vector</returns>
    {$ENDREGION}
    function DivAndAssign(const Value: Single): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Divides the content of another vector and assigns the result to this vector</summary>
    /// <param name="Other">Other vector to divide to this vector</param>
    /// <returns>Divided vector</returns>
    {$ENDREGION}
    function DivAndAssign(const Other: TQRVector2D): TQRVector2D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Gets the vector length</summary>
    /// <returns>Vector length</returns>
    {$ENDREGION}
    function Length: Single; inline;

    {$REGION 'Documentation'}
    /// <summary>Normalizes the vector</summary>
    /// <returns>Normalized vector</returns>
    {$ENDREGION}
    function Normalize: TQRVector2D; inline;

    {$REGION 'Documentation'}
    /// <summary>Calculates the cross product between 2 vectors</summary>
    /// <param name="Vector">Vector with which cross product is calculated</param>
    /// <returns>Cross product</returns>
    {$ENDREGION}
    function Cross(const Vector: TQRVector2D): TQRVector2D; inline;

    {$REGION 'Documentation'}
    /// <summary>Calculates the dot product between 2 vectors</summary>
    /// <param name="Vector">Vector with which dot product is calculated</param>
    /// <returns>Dot product</returns>
    {$ENDREGION}
    function Dot(const Vector: TQRVector2D): Single; inline;

    {$REGION 'Documentation'}
    /// <summary>Calculates interpolation vector between 2 vectors</summary>
    /// <param name="Other">Other vector to interpolate with</param>
    /// <param name="Position">Interpolation position, in percent (between 0.0 and 1.0)</param>
    /// <returns>Interpolation vector</returns>
    {$ENDREGION}
    function Interpolate(const Other: TQRVector2D; const Position: Single): TQRVector2D; inline;

    { Properties }
    property X: Single read FX write FX;
    property Y: Single read FY write FY;
  end;

  PQRVector2D = ^TQRVector2D;

  {$REGION 'Documentation'}
  /// <summary>Vector 3D</summary>
  {$ENDREGION}
  TQRVector3D = record
  private
    FX: Single; // x coordinate for the 3D vector
    FY: Single; // y coordinate for the 3D vector
    FZ: Single; // z coordinate for the 3D vector

  public
    {$REGION 'Documentation'}
    /// <summary>Constructor</summary>
    /// <param name="X">Vector x coordinate</summary>
    /// <param name="Y">Vector y coordinate</summary>
    /// <param name="Z">Vector z coordinate</summary>
    {$ENDREGION}
    constructor Create(const X, Y, Z: Single); overload;

    {$REGION 'Documentation'}
    /// <summary>Copy constructor</summary>
    /// <param name="Other">Other vector to copy from</summary>
    {$ENDREGION}
    constructor Create(const Other: TQRVector3D); overload;

    {$REGION 'Documentation'}
    /// <summary>Assigns (i.e. copies) the content from another vector</summary>
    /// <param name="Other">Other vector to copy from</param>
    {$ENDREGION}
    procedure Assign(const Other: TQRVector3D); inline;

    {$REGION 'Documentation'}
    /// <summary>Compares the content of 2 vectors and determines if they are equal</summary>
    /// <param name="Other">Other vector to compare with</param>
    /// <returns>True if vectors are equal, otherwise False</returns>
    {$ENDREGION}
    function IsEqual(const Other: TQRVector3D): Boolean; inline;

    {$REGION 'Documentation'}
    /// <summary>Compares the content of 2 vectors and determines if they are different</summary>
    /// <param name="Other">Other vector to compare with</param>
    /// <returns>True if vectors differ, otherwise False</returns>
    {$ENDREGION}
    function Differs(const Other: TQRVector3D): Boolean; inline;

    {$REGION 'Documentation'}
    /// <summary>Inverts the vector</summary>
    /// <returns>Inverted vector</returns>
    {$ENDREGION}
    function Invert: TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Adds a value to this vector</summary>
    /// <param name="Value">Value to add to vector</param>
    /// <returns>Added vector</returns>
    {$ENDREGION}
    function Add(const Value: Single): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Adds the content of another vector to this vector</summary>
    /// <param name="Other">Other vector to add to this vector</param>
    /// <returns>Added vector</returns>
    {$ENDREGION}
    function Add(const Other: TQRVector3D): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Subtracts a value to this vector</summary>
    /// <param name="Value">Value to subtract to this vector</param>
    /// <returns>Subtracted vector</returns>
    {$ENDREGION}
    function Sub(const Value: Single): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Subtracts to this vector the contents of another vector</summary>
    /// <param name="Other">Other vector to subtract to this vector</param>
    /// <returns>Subtracted vector</returns>
    {$ENDREGION}
    function Sub(const Other: TQRVector3D): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Multiplies a value to this vector</summary>
    /// <param name="Value">Value to multiply to this vector</param>
    /// <returns>Multiplied vector</returns>
    {$ENDREGION}
    function Mul(const Value: Single): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Multiplies the content of another vector to this vector</summary>
    /// <param name="Other">Other vector to multiply to this vector</param>
    /// <returns>Multiplied vector</returns>
    {$ENDREGION}
    function Mul(const Other: TQRVector3D): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Divides a value to this vector</summary>
    /// <param name="Value">Value to divide to this vector</param>
    /// <returns>Divided vector</returns>
    {$ENDREGION}
    function Divide(const Value: Single): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Divides the content of another vector to this vector</summary>
    /// <param name="Other">Other vector to divide to this vector</param>
    /// <returns>Divided vector</returns>
    {$ENDREGION}
    function Divide(const Other: TQRVector3D): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Adds a value and assigns the result to this vector</summary>
    /// <param name="Value">Value to add to vector</param>
    /// <returns>Added vector</returns>
    {$ENDREGION}
    function AddAndAssign(const Value: Single): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Adds the content of another vector and assigns the result to this vector</summary>
    /// <param name="Other">Other vector to add to this vector</param>
    /// <returns>Added vector</returns>
    {$ENDREGION}
    function AddAndAssign(const Other: TQRVector3D): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Subtracts a value and assigns the result to this vector</summary>
    /// <param name="Value">Value to subtract to vector</param>
    /// <returns>Subtracted vector</returns>
    {$ENDREGION}
    function SubAndAssign(const Value: Single): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Subtracts the content of another vector and assigns the result to this vector</summary>
    /// <param name="Other">Other vector to subtract to this vector</param>
    /// <returns>Subtracted vector</returns>
    {$ENDREGION}
    function SubAndAssign(const Other: TQRVector3D): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Multilpies a value and assigns the result to this vector</summary>
    /// <param name="Value">Value to multiply to vector</param>
    /// <returns>Multiplied vector</returns>
    {$ENDREGION}
    function MulAndAssign(const Value: Single): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Multiplies the content of another vector and assigns the result to this vector</summary>
    /// <param name="Other">Other vector to multiply to this vector</param>
    /// <returns>Multiplied vector</returns>
    {$ENDREGION}
    function MulAndAssign(const Other: TQRVector3D): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Divides a value and assigns the result to this vector</summary>
    /// <param name="Value">Value to divide to vector</param>
    /// <returns>Divided vector</returns>
    {$ENDREGION}
    function DivAndAssign(const Value: Single): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Divides the content of another vector and assigns the result to this vector</summary>
    /// <param name="Other">Other vector to divide to this vector</param>
    /// <returns>Divided vector</returns>
    {$ENDREGION}
    function DivAndAssign(const Other: TQRVector3D): TQRVector3D; overload; inline;

    {$REGION 'Documentation'}
    /// <summary>Gets the vector length</summary>
    /// <returns>Vector length</returns>
    {$ENDREGION}
    function Length: Single; inline;

    {$REGION 'Documentation'}
    /// <summary>Normalizes the vector</summary>
    /// <returns>Normalized vector</returns>
    {$ENDREGION}
    function Normalize: TQRVector3D; inline;

    {$REGION 'Documentation'}
    /// <summary>Calculates the cross product between 2 vectors</summary>
    /// <param name="Vector">Vector with which cross product is calculated</param>
    /// <returns>Cross product</returns>
    {$ENDREGION}
    function Cross(const Vector: TQRVector3D): TQRVector3D; inline;

    {$REGION 'Documentation'}
    /// <summary>Calculates the dot product between 2 vectors</summary>
    /// <param name="Vector">Vector with which dot product is calculated</param>
    /// <returns>Dot product</returns>
    {$ENDREGION}
    function Dot(const Vector: TQRVector3D): Single; inline;

    {$REGION 'Documentation'}
    /// <summary>Calculates interpolation vector between 2 vectors</summary>
    /// <param name="Other">Other vector to interpolate with</param>
    /// <param name="Position">Interpolation position, in percent (between 0.0 and 1.0)</param>
    /// <returns>Interpolation vector</returns>
    {$ENDREGION}
    function Interpolate(const Other: TQRVector3D; const Position: Single): TQRVector3D; inline;

    { Properties }
    property X: Single read FX write FX;
    property Y: Single read FY write FY;
    property Z: Single read FZ write FZ;
  end;

  PQRVector3D = ^TQRVector3D;

  {$REGION 'Documentation'}
  /// <summary>Plane</summary>
  {$ENDREGION}
  TQRPlane = record
  private
    FA: Single; // a coordinate for the plane
    FB: Single; // b coordinate for the plane
    FC: Single; // c coordinate for the plane
    FD: Single; // d coordinate for the plane

  public
    {$REGION 'Documentation'}
    /// <summary>Constructor</summary>
    /// <param name="A">Plane a coordinate (relative to formula aX + bY + cZ + d = 0)</summary>
    /// <param name="B">Plane b coordinate (relative to formula aX + bY + cZ + d = 0)</summary>
    /// <param name="C">Plane c coordinate (relative to formula aX + bY + cZ + d = 0)</summary>
    /// <param name="D">Plane d coordinate (relative to formula aX + bY + cZ + d = 0)</summary>
    {$ENDREGION}
    constructor Create(const A, B, C, D: Single); overload;

    {$REGION 'Documentation'}
    /// <summary>Copy constructor</summary>
    /// <param name="Other">Other plane to copy from</summary>
    {$ENDREGION}
    constructor Create(const Other: TQRPlane); overload;

    {$REGION 'Documentation'}
    /// <summary>Assigns (i.e. copies) the content from another plane</summary>
    /// <param name="Other">Other plane to copy from</param>
    {$ENDREGION}
    procedure Assign(const Other: TQRPlane); inline;

    {$REGION 'Documentation'}
    /// <summary>Compares the content of 2 planes and determines if they are equal</summary>
    /// <param name="Other">Other plane to compare with</param>
    /// <returns>True if planes are equal, otherwise False</returns>
    {$ENDREGION}
    function IsEqual(const Other: TQRPlane): Boolean; inline;

    {$REGION 'Documentation'}
    /// <summary>Compares the content of 2 planes and determines if they are different</summary>
    /// <param name="Other">Other plane to compare with</param>
    /// <returns>True if planes differ, otherwise False</returns>
    {$ENDREGION}
    function Differs(const Other: TQRPlane): Boolean; inline;

    {$REGION 'Documentation'}
    /// <summary>Inverts the plane</summary>
    /// <returns>Inverted plane</returns>
    {$ENDREGION}
    function Invert: TQRPlane; inline;

    {**
    * Calculates distance to plane
    *@param point - point from which the distance must be calculated
    *@return distance to plane
    *}
    function DistanceTo(const Point: TQRVector3D): Single; inline;

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
    *@param Other - other plane to compare
    *@param Tolerance - tolerance for comparison
    *@return true if planes are equals in the limits of the given tolerance, otherwise false
    *}
    function Compare(const Other: TQRPlane; const Tolerance: Single): Boolean; inline;

    {**
    * Calculates a plane using 3 vertex
    *@param V1 - value of the first vertex
    *@param V2 - value of the second vertex
    *@param V3 - value of the thrid vertex
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
    property A: Single read FA write FA;
    property B: Single read FB write FB;
    property C: Single read FC write FC;
    property D: Single read FD write FD;
  end;

  PQRPlane = ^TQRPlane;

  {**
  * 4x4 matrix
  *}
  TQRMatrix4x4 = record
  private
    FTable: array[0..3] of array [0..3] of Single; // 4x4 matrix array

    {**
    * Gets matrix item at index
    *@param Index - index
    *@return item
    *@throw exception if index is out of bounds
    *}
    function  GetItem(Index: NativeInt): Single; overload; inline;

    {**
    * Sets matrix item at index
    *@param Index - index
    *@param Value - value to set
    *@throw exception if index is out of bounds
    *}
    procedure SetItem(Index: NativeInt; Value: Single); overload; inline;

    {**
    * Gets matrix item from table
    *@param X - table x position
    *@param Y - table y position
    *@return item
    *@throw exception if positions are out of bounds
    *}
    function  GetTableItem(X, Y: NativeInt): Single; overload; inline;

    {**
    * Sets matrix item to table
    *@param X - table x position
    *@param Y - table y position
    *@param Value - value to set
    *@throw exception if positions are out of bounds
    *}
    procedure SetTableItem(X, Y: NativeInt; Value: Single); overload; inline;

  public
    {**
    * Constructor
    *@param _11 - [0][0] matrix table value
    *@param _12 - [1][0] matrix table value
    *@param _13 - [2][0] matrix table value
    *@param _14 - [3][0] matrix table value
    *@param _21 - [0][1] matrix table value
    *@param _22 - [1][1] matrix table value
    *@param _23 - [2][1] matrix table value
    *@param _24 - [3][1] matrix table value
    *@param _31 - [0][2] matrix table value
    *@param _32 - [1][2] matrix table value
    *@param _33 - [2][2] matrix table value
    *@param _34 - [3][2] matrix table value
    *@param _41 - [0][3] matrix table value
    *@param _42 - [1][3] matrix table value
    *@param _43 - [2][3] matrix table value
    *@param _44 - [3][3] matrix table value
    *}
    constructor Create(const _11, _12, _13, _14,
                             _21, _22, _23, _24,
                             _31, _32, _33, _34,
                             _41, _42, _43, _44: Single); overload;

    {**
    * Constructor
    *@param Other - other matrix to copy from
    *}
    constructor Create(const Other: TQRMatrix4x4); overload;

    {**
    * Gets record initialized with default values
    *@return record initialized with default values
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
    function Determinant: Single; inline;

    {**
    * Inverses the matrix
    *@param Determinant - determinant
    *@return inverse of the matrix
    *}
    function Inverse(out Determinant: Single): TQRMatrix4x4;

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
    function Rotate(const Angle: Single; const R: TQRVector3D): TQRMatrix4x4; inline;

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
    function GetPtr: PSingle; inline;

    {**
    * Gets an identity matrix
    *@return identity matrix
    *}
    class function Identity: TQRMatrix4x4; static; inline;

    { Properties }
    property Item[Index: NativeInt]: Single read GetItem      write SetItem;
    property Table[X, Y: NativeInt]: Single read GetTableItem write SetTableItem;
    property Ptr: PSingle                   read GetPtr;
  end;

  PQRMatrix4x4 = ^TQRMatrix4x4;

  {**
  * Quaternion
  *}
  TQRQuaternion = record
  private
    FX: Single; // x coordinate for the quaternion
    FY: Single; // y coordinate for the quaternion
    FZ: Single; // z coordinate for the quaternion
    FW: Single; // w coordinate for the quaternion

  public
    {**
    * Constructor
    *@param X - vector x value
    *@param Y - vector y value
    *@param Z - vector z value
    *@param W - vector w value
    *}
    constructor Create(X, Y, Z, W: Single); overload;

    {**
    * Constructor that creates a quaternion from an axis and an angle
    *@param Vector - vector representing axis
    *@param Angle - angle in radians
    *}
    constructor Create(const Vector: TQRVector3D; Angle: Single); overload;

    {**
    * Constructor that creates a quaternion from a 4x4 matrix
    *@param matrix - matrix
    *}
    constructor Create(const Matrix: TQRMatrix4x4); overload;

    {**
    * Copy constructor
    *@param Other - other quaternion to copy from
    *}
    constructor Create(const Other: TQRQuaternion); overload;

    { Basic functions }
    procedure Assign(const Other: TQRQuaternion);                                inline;
    function  IsEqual(const Other: TQRQuaternion): Boolean;                      inline;
    function  Differs(const Other: TQRQuaternion): Boolean;                      inline;
    function  Invert: TQRQuaternion;                                   overload; inline;
    function  Add(const Value: Single): TQRQuaternion;                 overload; inline;
    function  Add(const Other: TQRQuaternion): TQRQuaternion;          overload; inline;
    function  Sub(const Value: Single): TQRQuaternion;                 overload; inline;
    function  Sub(const Other: TQRQuaternion): TQRQuaternion;          overload; inline;
    function  Mul(const Value: Single): TQRQuaternion;                 overload; inline;
    function  Mul(const Other: TQRQuaternion): TQRQuaternion;          overload; inline;
    function  Divide(const Value: Single): TQRQuaternion;              overload; inline;
    function  Divide(const Other: TQRQuaternion): TQRQuaternion;       overload; inline;
    function  AddAndAssign(const Value: Single): TQRQuaternion;        overload; inline;
    function  AddAndAssign(const Other: TQRQuaternion): TQRQuaternion; overload; inline;
    function  SubAndAssign(const Value: Single): TQRQuaternion;        overload; inline;
    function  SubAndAssign(const Other: TQRQuaternion): TQRQuaternion; overload; inline;
    function  MulAndAssign(const Value: Single): TQRQuaternion;        overload; inline;
    function  MulAndAssign(const Other: TQRQuaternion): TQRQuaternion; overload; inline;
    function  DivAndAssign(const Value: Single): TQRQuaternion;        overload; inline;
    function  DivAndAssign(const Other: TQRQuaternion): TQRQuaternion; overload; inline;

    {**
    * Calculates the norm of the quaternion
    *@return the norm of the quaternion
    *}
    function Norm: Single; inline;

    {**
    * Gets the quaternion length
    *@return the quaternion length
    *}
    function Length: Single; inline;

    {**
    * Normalizes the vector
    *@return normalized vector
    *}
    function Normalize: TQRQuaternion; inline;

    {**
    * Calculates the dot product between 2 quaternions
    *@param Q - quaternion with which dot product is calculated
    *@return dot product
    *}
    function Dot(const Q: TQRQuaternion): Single; inline;

    {**
    * Scales the quaternion
    *@param S - scale factor to apply
    *@return scaled quaternion
    *}
    function Scale(S: Single): TQRQuaternion; inline;

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
    *@param Other - other quaternion to interpolate with
    *@param p - interpolation position, in percent (between 0.0f and 1.0f)
    *@return the spherical linear interpolated quaternion
    *}
    function Slerp(const Other: TQRQuaternion; P: Single): TQRQuaternion; inline;

    {**
    * Rotates a vector by the quaternion
    *@param Vector - vector to rotate
    *@return rotated vector
    *}
    function Rotate(const Vector: TQRVector3D): TQRVector3D; inline;

    {**
    * Gets matrix from quaternion
    *@return matrix
    *}
    function GetMatrix: TQRMatrix4x4; inline;


    { Properties }
    property X: Single read FX write FX;
    property Y: Single read FY write FY;
    property Z: Single read FZ write FZ;
    property W: Single read FW write FW;
  end;

  PQRQuaternion = ^TQRQuaternion;

  {**
  * Ray
  *}
  TQRRay = class
  private
    FPos:    TQRVector3D;
    FDir:    TQRVector3D;
    FInvDir: TQRVector3D;

    {**
    * Gets position
    *@return position
    *}
    function GetPos: PQRVector3D; virtual;

    {**
    * Sets position
    *@param PtPos - position
    *}
    procedure SetPos(const PtPos: PQRVector3D); virtual;

    {**
    * Gets direction
    *@return direction
    *}
    function GetDir: PQRVector3D; virtual;

    {**
    * Sets direction
    *@param PtDir - direction
    *}
    procedure SetDir(const PtDir: PQRVector3D); virtual;

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
    *@param PtOther - other ray to copy from
    *}
    constructor Create(const PtOther: TQRRay); overload; virtual;

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
    FVertex: array[0..2] of TQRVector3D;

    {**
    * Checks if a vector is between start and end limits
    *@param Value - value to check
    *@param VStart - start limit
    *@param VEnd - end limit
    *@param Tolerance - tolerance
    *@return true if value is between limits, otherwise false
    *}
    function IsBetween(const Value, VStart, VEnd: TQRVector3D;
                                       Tolerance: Single): Boolean; overload;

    {**
    * Checks if a value is between start and end limits
    *@param Value - value to check
    *@param LStart - start limit
    *@param LEnd - end limit
    *@param Tolerance - tolerance
    *@return true if value is between limits, otherwise false
    *}
    function IsBetween(const Value, LStart, LEnd, Tolerance: Single): Boolean; overload;

  public
    {**
    * Constructor
    *@param Vertex1 - first vertex of the polygon
    *@param Vertex2 - second vertex of the polygon
    *@param Vertex3 - third vertex of the polygon
    *}
    constructor Create(const Vertex1, Vertex2, Vertex3: TQRVector3D);

    {**
    * Gets vertex at index
    *@param Index - vertex index
    *@return corresponding vertex, nil if not found
    *}
     function GetVertex(Index: Byte): TQRVector3D;

    {**
    * Sets vertex
    *@param Index - vertex index to set
    *@param Vertex - vertex value
    *}
    procedure SetVertex(Index: Byte; const Vertex: TQRVector3D);

    {**
    * Gets first polygon vertex
    *@return first polygon vertex
    *}
    function GetVertex1: PQRVector3D;

    {**
    * Sets first polygon vertex
    *@param PtVertex - first polygon vertex value
    *}
    procedure SetVertex1(const PtVertex: PQRVector3D);

    {**
    * Gets second polygon vertex
    *@return second polygon vertex
    *}
    function GetVertex2: PQRVector3D;

    {**
    * Sets second polygon vertex
    *@param PtVertex - second polygon vertex value
    *}
    procedure SetVertex2(const PtVertex: PQRVector3D);

    {**
    * Gets third polygon vertex
    *@return third polygon vertex
    *}
    function GetVertex3: PQRVector3D;

    {**
    * Sets third polygon vertex
    *@param PtVertex - third polygon vertex value
    *}
    procedure SetVertex3(const PtVertex: PQRVector3D);

    {**
    * Creates and returns a clone of the polygon
    *@return a clone of the polygon
    *@note The returned polygon should be deleted when useless
    *}
    function GetClone: TQRPolygon;

    {**
    * Applies the given matrix to the polygon
    *@param Matrix - matrix to apply
    *@return transformed polygon
    *@note The returned polygon should be deleted when useless
    *}
    function ApplyMatrix(const Matrix: TQRMatrix4x4): TQRPolygon;

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
    *@param X - point x coordinate
    *@param Y - point y coordinate
    *@param Z - point z coordinate
    *@return true if point is inside polygon, otherwise false
    *}
    function Inside(const X, Y, Z: Single): Boolean; overload;

    {**
    * Checks if a point is inside polygon
    *@param Point - point coordinate
    *@return true if point is inside polygon, otherwise false
    *}
    function Inside(const Point: TQRVector3D): Boolean; overload;

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
    FRadius: Single;

    {**
    * Gets circle center pos
    *@return sphere center pos
    *}
    function GetPos: PQRVector2D;

    {**
    * Sets circle center pos
    *@param PtPos - sphere center pos
    *}
    procedure SetPos(const PtPos: PQRVector2D);

  public
    { Properties }
    property Pos:    PQRVector2D read GetPos  write SetPos;
    property Radius: Single      read FRadius write FRadius;
  end;

  {**
  * 3D sphere
  *}
  TQRSphere = record
  private
    FPos:    TQRVector3D;
    FRadius: Single;

    {**
    * Gets sphere center pos
    *@return sphere center pos
    *}
    function GetPos: PQRVector3D;

    {**
    * Sets sphere center pos
    *@param PtPos - sphere center pos
    *}
    procedure SetPos(const PtPos: PQRVector3D);

  public
    { Properties }
    property Pos:    PQRVector3D read GetPos  write SetPos;
    property Radius: Single      read FRadius write FRadius;
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
    *@param PtValue - box min edge
    *}
    procedure SetMin(const PtValue: PQRVector2D);

    {**
    * Gets rect max edge
    *@return box max edge
    *}
    function GetMax: PQRVector2D;

    {**
    * Sets rect max edge
    *@param PtValue - box max edge
    *}
    procedure SetMax(const PtValue: PQRVector2D);

  public
    {**
    * Constructor
    *@param X - rect x position, i.e. the position of the left edge
    *@param Y - rect y position, i.e. the position of the top edge
    *@param Width - width
    *@param Height - height
    *}
    constructor Create(const X, Y, Width, Height: Single);

    {**
    * Gets width
    *@return width
    *}
    function GetWidth: Single;

    {**
    * Gets height
    *@return height
    *}
    function GetHeight: Single;

    { Properties }
    property Min:    PQRVector2D read GetMin write SetMin;
    property Max:    PQRVector2D read GetMax write SetMax;
    property Width:  Single      read GetWidth;
    property Height: Single      read GetHeight;
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
    *@param PtValue - box min edge
    *}
    procedure SetMin(const PtValue: PQRVector3D);

    {**
    * Gets box max edge
    *@return box max edge
    *}
    function GetMax: PQRVector3D;

    {**
    * Sets box max edge
    *@param PtValue - box max edge
    *}
    procedure SetMax(const PtValue: PQRVector3D);

  public
    {**
    * Cuts box on the longest axis
    *@param[in, out] LeftBox - resulting left box
    *@param[in, out] RightBox - resulting right box
    *}
    procedure Cut(var LeftBox, RightBox: TQRBox);

    { Properties }
    property Min: PQRVector3D read GetMin write SetMin;
    property Max: PQRVector3D read GetMax write SetMax;
  end;

  PQRBox = ^TQRBox;

implementation
//------------------------------------------------------------------------------
// TQRVector2D
//------------------------------------------------------------------------------
constructor TQRVector2D.Create(const X, Y: Single);
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
function TQRVector2D.Invert: TQRVector2D;
begin
  result := TQRVector2D.Create(-FX, -FY);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Add(const Value: Single): TQRVector2D;
begin
  result := TQRVector2D.Create(FX + Value, FY + Value);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Add(const Other: TQRVector2D): TQRVector2D;
begin
  result := TQRVector2D.Create(FX + Other.FX, FY + Other.FY);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Sub(const Value: Single): TQRVector2D;
begin
  Result := TQRVector2D.Create(FX - Value, FY - Value);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Sub(const Other: TQRVector2D): TQRVector2D;
begin
  Result := TQRVector2D.Create(FX - Other.FX, FY - Other.FY);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Mul(const Value: Single): TQRVector2D;
begin
  Result := TQRVector2D.Create(FX * Value, FY * Value);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Mul(const Other: TQRVector2D): TQRVector2D;
begin
  Result := TQRVector2D.Create(FX * Other.FX, FY * Other.FY);
end;
//------------------------------------------------------------------------------
function TQRVector2D.Divide(const Value: Single): TQRVector2D;
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
function TQRVector2D.AddAndAssign(const Value: Single): TQRVector2D;
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
function TQRVector2D.SubAndAssign(const Value: Single): TQRVector2D;
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
function TQRVector2D.MulAndAssign(const Value: Single): TQRVector2D;
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
function TQRVector2D.DivAndAssign(const Value: Single): TQRVector2D;
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
function TQRVector2D.Length: Single;
begin
  Result := Sqrt((FX * FX) + (FY * FY));
end;
//------------------------------------------------------------------------------
function TQRVector2D.Normalize: TQRVector2D;
var
  Len: Single;
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
function TQRVector2D.Dot(const Vector: TQRVector2D): Single;
begin
  Result := ((FX * Vector.FX) + (FY * Vector.FY));
end;
//------------------------------------------------------------------------------
function TQRVector2D.Interpolate(const Other: TQRVector2D; const Position: Single): TQRVector2D;
begin
  // is position out of bounds? Limit to min or max values in this case
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
// TQRVector3D
//------------------------------------------------------------------------------
constructor TQRVector3D.Create(const X, Y, Z: Single);
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
function TQRVector3D.Invert: TQRVector3D;
begin
  Result := TQRVector3D.Create(-FX, -FY, -FZ);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Add(const Value: Single): TQRVector3D;
begin
  Result := TQRVector3D.Create(FX + Value, FY + Value, FZ + Value);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Add(const Other: TQRVector3D): TQRVector3D;
begin
  Result := TQRVector3D.Create(FX + Other.FX, FY + Other.FY, FZ + Other.FZ);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Sub(const Value: Single): TQRVector3D;
begin
  Result := TQRVector3D.Create(FX - Value, FY - Value, FZ - Value);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Sub(const Other: TQRVector3D): TQRVector3D;
begin
  Result := TQRVector3D.Create(FX - Other.FX, FY - Other.FY, FZ - Other.FZ);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Mul(const Value: Single): TQRVector3D;
begin
  Result := TQRVector3D.Create(FX * Value, FY * Value, FZ * Value);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Mul(const Other: TQRVector3D): TQRVector3D;
begin
  Result := TQRVector3D.Create(FX * Other.FX, FY * Other.FY, FZ * Other.FZ);
end;
//------------------------------------------------------------------------------
function TQRVector3D.Divide(const Value: Single): TQRVector3D;
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
function TQRVector3D.AddAndAssign(const Value: Single): TQRVector3D;
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
function TQRVector3D.SubAndAssign(const Value: Single): TQRVector3D;
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
function TQRVector3D.MulAndAssign(const Value: Single): TQRVector3D;
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
function TQRVector3D.DivAndAssign(const Value: Single): TQRVector3D;
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
function TQRVector3D.Length: Single;
begin
  Result := Sqrt((FX * FX) + (FY * FY) + (FZ * FZ));
end;
//------------------------------------------------------------------------------
function TQRVector3D.Normalize: TQRVector3D;
var
  Len: Single;
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
function TQRVector3D.Dot(const vector: TQRVector3D): Single;
begin
  Result := ((FX * Vector.FX) + (FY * Vector.FY) + (FZ * Vector.FZ));
end;
//------------------------------------------------------------------------------
function TQRVector3D.Interpolate(const Other: TQRVector3D; const Position: Single): TQRVector3D;
begin
  // is position out of bounds? Limit to min or max values in this case
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
// TQRPlane
//------------------------------------------------------------------------------
constructor TQRPlane.Create(const A, B, C, D: Single);
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
function TQRPlane.Invert: TQRPlane;
begin
  Result := TQRPlane.Create(-FA, -FB, -FC, -FD);
end;
//------------------------------------------------------------------------------
function TQRPlane.DistanceTo(const Point: TQRVector3D): Single;
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
  Dot, Temp: Single;
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
function TQRPlane.Compare(const Other: TQRPlane; const Tolerance: Single): Boolean;
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
// TQRMatrix4x4
//------------------------------------------------------------------------------
constructor TQRMatrix4x4.Create(const _11, _12, _13, _14,
                                      _21, _22, _23, _24,
                                      _31, _32, _33, _34,
                                      _41, _42, _43, _44: Single);
begin
  FTable[0][0] := _11; FTable[0][1] := _12; FTable[0][2] := _13; FTable[0][3] := _14;
  FTable[1][0] := _21; FTable[1][1] := _22; FTable[1][2] := _23; FTable[1][3] := _24;
  FTable[2][0] := _31; FTable[2][1] := _32; FTable[2][2] := _33; FTable[2][3] := _34;
  FTable[3][0] := _41; FTable[3][1] := _42; FTable[3][2] := _43; FTable[3][3] := _44;
end;
//------------------------------------------------------------------------------
constructor TQRMatrix4x4.Create(const Other: TQRMatrix4x4);
begin
  Assign(Other);
end;
//------------------------------------------------------------------------------
class function TQRMatrix4x4.GetDefault: TQRMatrix4x4;
begin
  Result := Identity;
end;
//------------------------------------------------------------------------------
function TQRMatrix4x4.GetItem(Index: NativeInt): Single;
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
//------------------------------------------------------------------------------
procedure TQRMatrix4x4.SetItem(Index: NativeInt; Value: Single);
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
//------------------------------------------------------------------------------
function TQRMatrix4x4.GetTableItem(X, Y: NativeInt): Single;
begin
  if (X > 3) then
    raise Exception.Create('X index is out of bounds');

  if (Y > 3) then
    raise Exception.Create('Y index is out of bounds');

  Result := FTable[X][Y];
end;
//------------------------------------------------------------------------------
procedure TQRMatrix4x4.SetTableItem(X, Y: NativeInt; Value: Single);
begin
  if (X > 3) then
    raise Exception.Create('X index is out of bounds');

  if (Y > 3) then
    raise Exception.Create('Y index is out of bounds');

  FTable[X][Y] := Value;
end;
//------------------------------------------------------------------------------
procedure TQRMatrix4x4.Assign(const Other: TQRMatrix4x4);
var
  I, J: Byte;
begin
  // copy matrix table from other
  for I := 0 to 3 do
    for J := 0 to 3 do
      FTable[I][J] := Other.FTable[I][J];
end;
//------------------------------------------------------------------------------
function TQRMatrix4x4.IsEqual(const Other: TQRMatrix4x4): Boolean;
var
  I, J: Byte;
begin
  // compare each matrix element with other matrix
  for I := 0 to 3 do
    for J := 0 to 3 do
      if (FTable[I][J] <> Other.FTable[I][J]) then
      begin
        Result := False;
        Exit;
      end;

  Result := True;
end;
//------------------------------------------------------------------------------
function TQRMatrix4x4.Differs(const Other: TQRMatrix4x4): Boolean;
begin
  Result := not IsEqual(Other);
end;
//------------------------------------------------------------------------------
function TQRMatrix4x4.IsIdentity: Boolean;
begin
  Result := IsEqual(Identity);
end;
//------------------------------------------------------------------------------
function TQRMatrix4x4.Determinant: Single;
var
  T: array [0..2] of Single;
  V: array [0..3] of Single;
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
//------------------------------------------------------------------------------
function TQRMatrix4x4.Inverse(out Determinant: Single): TQRMatrix4x4;
var
  InvDet: Single;
  T:      array [0..2]  of Single;
  V:      array [0..15] of Single;
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
//------------------------------------------------------------------------------
function TQRMatrix4x4.Multiply(const Other: TQRMatrix4x4): TQRMatrix4x4;
var
  I, J: Byte;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result.FTable[I][J] := FTable[I][0] * Other.FTable[0][J] +
                             FTable[I][1] * Other.FTable[1][J] +
                             FTable[I][2] * Other.FTable[2][J] +
                             FTable[I][3] * Other.FTable[3][J];
end;
//------------------------------------------------------------------------------
function TQRMatrix4x4.Translate(const T: TQRVector3D): TQRMatrix4x4;
begin
  FTable[3][0] := FTable[3][0] + (FTable[0][0] * T.FX + FTable[1][0] * T.FY + FTable[2][0] * T.FZ);
  FTable[3][1] := FTable[3][1] + (FTable[0][1] * T.FX + FTable[1][1] * T.FY + FTable[2][1] * T.FZ);
  FTable[3][2] := FTable[3][2] + (FTable[0][2] * T.FX + FTable[1][2] * T.FY + FTable[2][2] * T.FZ);
  FTable[3][3] := FTable[3][3] + (FTable[0][3] * T.FX + FTable[1][3] * T.FY + FTable[2][3] * T.FZ);

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRMatrix4x4.Rotate(const Angle: Single; const R: TQRVector3D): TQRMatrix4x4;
var
  C, S, Ic: Single;
  Matrix:   TQRMatrix4x4;
begin
  // calculate sinus, cosinus and inverted cosinus values
  C  := Cos(Angle);
  S  := Sin(Angle);
  Ic := (1.0 - C);

  // create rotation matrix
  Matrix               := Identity;
  Matrix.FTable[0][0] := (Ic * R.FX * R.FX) +  C;
  Matrix.FTable[1][0] := (Ic * R.FX * R.FY) - (S * R.FZ);
  Matrix.FTable[2][0] := (Ic * R.FX * R.FZ) + (S * R.FY);
  Matrix.FTable[0][1] := (Ic * R.FY * R.FX) + (S * R.FZ);
  Matrix.FTable[1][1] := (Ic * R.FY * R.FY) +  C;
  Matrix.FTable[2][1] := (Ic * R.FY * R.FZ) - (S * R.FX);
  Matrix.FTable[0][2] := (Ic * R.FZ * R.FX) - (S * R.FY);
  Matrix.FTable[1][2] := (Ic * R.FZ * R.FY) + (S * R.FX);
  Matrix.FTable[2][2] := (Ic * R.FZ * R.FZ) +  C;

  // combine current matrix with rotation matrix
  Self := Matrix.Multiply(Self);

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRMatrix4x4.Scale(const S: TQRVector3D): TQRMatrix4x4;
begin
  FTable[0][0] := FTable[0][0] * S.FX; FTable[1][0] := FTable[1][0] * S.FY; FTable[2][0] := FTable[2][0] * S.FZ;
  FTable[0][1] := FTable[0][1] * S.FX; FTable[1][1] := FTable[1][1] * S.FY; FTable[2][1] := FTable[2][1] * S.FZ;
  FTable[0][2] := FTable[0][2] * S.FX; FTable[1][2] := FTable[1][2] * S.FY; FTable[2][2] := FTable[2][2] * S.FZ;
  FTable[0][3] := FTable[0][3] * S.FX; FTable[1][3] := FTable[1][3] * S.FY; FTable[2][3] := FTable[2][3] * S.FZ;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRMatrix4x4.Swap: TQRMatrix4x4;
begin
  Result := TQRMatrix4x4.Create(FTable[0][0], FTable[1][0], FTable[2][0], FTable[3][0],
                                FTable[0][1], FTable[1][1], FTable[2][1], FTable[3][1],
                                FTable[0][2], FTable[1][2], FTable[2][2], FTable[3][2],
                                FTable[0][3], FTable[1][3], FTable[2][3], FTable[3][3]);
end;
//------------------------------------------------------------------------------
function TQRMatrix4x4.Transform(const Vector: TQRVector3D): TQRVector3D;
begin
  // calculates x, y and z coordinates (don't use w component), and returns
  // transformed vector
  Result := TQRVector3D.Create((Vector.FX * FTable[0][0] +
                                Vector.FY * FTable[1][0] +
                                Vector.FZ * FTable[2][0] +
                                FTable[3][0]),
                               (Vector.FX * FTable[0][1] +
                                Vector.FY * FTable[1][1] +
                                Vector.FZ * FTable[2][1] +
                                FTable[3][1]),
                               (Vector.FX * FTable[0][2] +
                                Vector.FY * FTable[1][2] +
                                Vector.FZ * FTable[2][2] +
                                FTable[3][2]));
end;
//------------------------------------------------------------------------------
function TQRMatrix4x4.GetPtr: PSingle;
begin
  Result := @FTable[0][0];
end;
//------------------------------------------------------------------------------
class function TQRMatrix4x4.Identity: TQRMatrix4x4;
begin
  Result := TQRMatrix4x4.Create(1.0, 0.0, 0.0, 0.0,
                                0.0, 1.0, 0.0, 0.0,
                                0.0, 0.0, 1.0, 0.0,
                                0.0, 0.0, 0.0, 1.0);
end;
//------------------------------------------------------------------------------
// TQRQuaternion
//------------------------------------------------------------------------------
constructor TQRQuaternion.Create(X, Y, Z, W: Single);
begin
  FX := X;
  FY := Y;
  FZ := Z;
  FW := W;
end;
//------------------------------------------------------------------------------
constructor TQRQuaternion.Create(const Vector: TQRVector3D; Angle: Single);
var
  SinAngle: Single;
begin
  SinAngle := Sin(Angle);

  FX := (Vector.FX * SinAngle);
  FY := (Vector.FY * SinAngle);
  FZ := (Vector.FZ * SinAngle);
  FW :=  Cos(Angle);
end;
//------------------------------------------------------------------------------
constructor TQRQuaternion.Create(const Matrix: TQRMatrix4x4);
var
 Diagonal, Scale: Single;
begin
  // calculate the matrix diagonal by adding up it's diagonal indices (also known as "trace")
  Diagonal := Matrix.FTable[0][0] +
              Matrix.FTable[1][1] +
              Matrix.FTable[2][2] +
              Matrix.FTable[3][3];

  // is diagonal greater than zero?
  if (Diagonal > 0.00000001) then
  begin
    // calculate the diagonal Scale
    Scale := Sqrt(Diagonal) * 2.0;

    // calculate the quaternion values using the respective equation
    FX := (Matrix.FTable[1][2] - Matrix.FTable[2][1]) / Scale;
    FY := (Matrix.FTable[2][0] - Matrix.FTable[0][2]) / Scale;
    FZ := (Matrix.FTable[0][1] - Matrix.FTable[1][0]) / Scale;
    FW := 0.25 * Scale;

    Exit;
  end;

  // search for highest value in the matrix diagonal
  if ((Matrix.FTable[0][0] > Matrix.FTable[1][1]) and (Matrix.FTable[0][0] > Matrix.FTable[2][2])) then
  begin
    // calculate scale using the first diagonal element and double that value
    Scale := Sqrt(1.0 + Matrix.FTable[0][0] - Matrix.FTable[1][1] - Matrix.FTable[2][2]) * 2.0;

    // calculate the quaternion values using the respective equation
    FX := 0.25 * Scale;
    FY := (Matrix.FTable[0][1] + Matrix.FTable[1][0]) / Scale;
    FZ := (Matrix.FTable[2][0] + Matrix.FTable[0][2]) / Scale;
    FW := (Matrix.FTable[1][2] - Matrix.FTable[2][1]) / Scale;
  end
  else
  if (Matrix.FTable[1][1] > Matrix.FTable[2][2]) then
  begin
    // calculate scale using the second diagonal element and double that value
    Scale := Sqrt(1.0 + Matrix.FTable[1][1] - Matrix.FTable[0][0] - Matrix.FTable[2][2]) * 2.0;

    // calculate the quaternion values using the respective equation
    FX := (Matrix.FTable[0][1] + Matrix.FTable[1][0]) / Scale;
    FY := 0.25 * Scale;
    FZ := (Matrix.FTable[1][2] + Matrix.FTable[2][1]) / Scale;
    FW := (Matrix.FTable[2][0] - Matrix.FTable[0][2]) / Scale;
  end
  else
  begin
    // calculate scale using the third diagonal element and double that value
    Scale := Sqrt(1.0 + Matrix.FTable[2][2] - Matrix.FTable[0][0] - Matrix.FTable[1][1]) * 2.0;

    // calculate the quaternion values using the respective equation
    FX := (Matrix.FTable[2][0] + Matrix.FTable[0][2]) / Scale;
    FY := (Matrix.FTable[1][2] + Matrix.FTable[2][1]) / Scale;
    FZ := 0.25 * Scale;
    FW := (Matrix.FTable[0][1] - Matrix.FTable[1][0]) / Scale;
  end
end;
//------------------------------------------------------------------------------
constructor TQRQuaternion.Create(const Other: TQRQuaternion);
begin
  Assign(Other);
end;
//------------------------------------------------------------------------------
procedure TQRQuaternion.Assign(const Other: TQRQuaternion);
begin
  FX := Other.FX;
  FY := Other.FY;
  FZ := Other.FZ;
  FW := Other.FW;
end;
//------------------------------------------------------------------------------
function TQRQuaternion.IsEqual(const Other: TQRQuaternion): Boolean;
begin
  Result := ((FX = Other.FX) and (FY = Other.FY) and (FZ = Other.FZ) and (FW = Other.FW));
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Differs(const Other: TQRQuaternion): Boolean;
begin
  Result := ((FX <> Other.FX) or (FY <> Other.FY) or (FZ <> Other.FZ) or (FW <> Other.FW));
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Invert: TQRQuaternion;
begin
  Result := TQRQuaternion.Create(-FX, -FY, -FZ, -FW);
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Add(const Value: Single): TQRQuaternion;
begin
  Result := TQRQuaternion.Create(FX + Value, FY + Value, FZ + Value, FW + Value);
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Add(const Other: TQRQuaternion): TQRQuaternion;
begin
  Result := TQRQuaternion.Create(FX + Other.FX,
                                 FY + Other.FY,
                                 FZ + Other.FZ,
                                 FW + Other.FW);
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Sub(const Value: Single): TQRQuaternion;
begin
  Result := TQRQuaternion.Create(FX - Value, FY - Value, FZ - Value, FW - Value);
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Sub(const Other: TQRQuaternion): TQRQuaternion;
begin
  Result := TQRQuaternion.Create(FX - Other.FX,
                                 FY - Other.FY,
                                 FZ - Other.FZ,
                                 FW - Other.FW);
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Mul(const Value: Single): TQRQuaternion;
begin
  Result := TQRQuaternion.Create(FX * Value, FY * Value, FZ * Value, FW * Value);
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Mul(const Other: TQRQuaternion): TQRQuaternion;
begin
  Result := TQRQuaternion.Create(FX * Other.FX,
                                 FY * Other.FY,
                                 FZ * Other.FZ,
                                 FW * Other.FW);
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Divide(const Value: Single): TQRQuaternion;
begin
  if (Value = 0.0) then
    raise Exception.Create('Division by 0 is prohibited');

  Result := TQRQuaternion.Create(FX / Value, FY / Value, FZ / Value, FW / Value);
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Divide(const Other: TQRQuaternion): TQRQuaternion;
begin
  if (Other.FX = 0.0) then
    raise Exception.Create('Quaternion x value - division by 0 is prohibited');

  if (Other.FY = 0.0) then
    raise Exception.Create('Quaternion y value - division by 0 is prohibited');

  if (Other.FZ = 0.0) then
    raise Exception.Create('Quaternion z value - division by 0 is prohibited');

  if (Other.FW = 0.0) then
    raise Exception.Create('Quaternion w value - division by 0 is prohibited');

  Result := TQRQuaternion.Create(FX / Other.FX,
                                 FY / Other.FY,
                                 FZ / Other.FZ,
                                 FW / Other.FW);
end;
//------------------------------------------------------------------------------
function TQRQuaternion.AddAndAssign(const Value: Single): TQRQuaternion;
begin
  FX := FX + Value;
  FY := FY + Value;
  FZ := FZ + Value;
  FW := FW + Value;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRQuaternion.AddAndAssign(const Other: TQRQuaternion): TQRQuaternion;
begin
  FX := FX + Other.FX;
  FY := FY + Other.FY;
  FZ := FZ + Other.FZ;
  FW := FW + Other.FW;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRQuaternion.SubAndAssign(const Value: Single): TQRQuaternion;
begin
  FX := FX - Value;
  FY := FY - Value;
  FZ := FZ - Value;
  FW := FW - Value;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRQuaternion.SubAndAssign(const Other: TQRQuaternion): TQRQuaternion;
begin
  FX := FX - Other.FX;
  FY := FY - Other.FY;
  FZ := FZ - Other.FZ;
  FW := FW - Other.FW;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRQuaternion.MulAndAssign(const Value: Single): TQRQuaternion;
begin
  FX := FX * Value;
  FY := FY * Value;
  FZ := FZ * Value;
  FW := FW * Value;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRQuaternion.MulAndAssign(const Other: TQRQuaternion): TQRQuaternion;
begin
  FX := FX * Other.FX;
  FY := FY * Other.FY;
  FZ := FZ * Other.FZ;
  FW := FW * Other.FW;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRQuaternion.DivAndAssign(const Value: Single): TQRQuaternion;
begin
  if (Value = 0.0) then
    raise Exception.Create('Division by 0 is prohibited');

  FX := FX / Value;
  FY := FY / Value;
  FZ := FZ / Value;
  FW := FW / Value;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRQuaternion.DivAndAssign(const Other: TQRQuaternion): TQRQuaternion;
begin
  if (Other.FX = 0.0) then
    raise Exception.Create('Quaternion x value - division by 0 is prohibited');

  if (Other.FY = 0.0) then
    raise Exception.Create('Quaternion y value - division by 0 is prohibited');

  if (Other.FZ = 0.0) then
    raise Exception.Create('Quaternion z value - division by 0 is prohibited');

  if (Other.FW = 0.0) then
    raise Exception.Create('Quaternion w value - division by 0 is prohibited');

  FX := FX / Other.FX;
  FY := FY / Other.FY;
  FZ := FZ / Other.FZ;
  FW := FW / Other.FW;

  Result := Self;
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Norm: Single;
begin
  Result := ((FX * FX) + (FY * FY) + (FZ * FZ) + (FW * FW));
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Length: Single;
begin
  Result := Sqrt(Norm);
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Normalize: TQRQuaternion;
var
  Len: Single;
begin
  Len := Length;

  if (Len = 0.0) then
  begin
    Result := Default(TQRQuaternion);
    Exit;
  end;

  Result := TQRQuaternion.Create((FX / Len), (FY / Len), (FZ / Len), (FW / Len));
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Dot(const Q: TQRQuaternion): Single;
begin
  Result := ((FX * Q.FX) + (FY * Q.FY) + (FZ * Q.FZ) + (FW * Q.FW));
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Scale(S: Single): TQRQuaternion;
begin
  Result := TQRQuaternion.Create(FX * S, FY * S, FZ * S, FW * S);
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Conjugate: TQRQuaternion;
begin
  Result := TQRQuaternion.Create(-FX, -FY, -FZ, FW);
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Inverse: TQRQuaternion;
var
  QuatNorm: Single;
begin
  // calculate the norm of the quaternion
  QuatNorm := Norm;

  // empty quaternion?
  if (QuatNorm = 0.0) then
  begin
    Result := Default(TQRQuaternion);
    Exit;
  end;

  Result := Conjugate.Scale(1.0 / QuatNorm);
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Slerp(const Other: TQRQuaternion; P: Single): TQRQuaternion;
var
  QuatDot, Scale0, Scale1, Theta, SinTheta: Single;
  InterpolateWith:                          TQRQuaternion;
begin
  // are quaternions identical?
  if (IsEqual(Other)) then
  begin
    Result := Self;
    Exit;
  end;

  // calculate dot product between q1 and q2
  QuatDot := Dot(Other);

  // check if angle is higher than 90° (this happen if dot product is less than 0)
  if (QuatDot < 0.0) then
  begin
    // negate the second quaternion and the dot product result
    InterpolateWith :=  Other.Invert;
    QuatDot         := -QuatDot;
  end
  else
    InterpolateWith := Other;

  // calculate the interpolation first and second scale
  Scale0 := 1.0 - P;
  Scale1 := P;

  // is angle large enough to apply the calculation
  if ((1.0 - QuatDot) > 0.1) then
  begin
    // calculate the angle between the 2 quaternions and get the sinus of that angle
    Theta    := ArcCos(QuatDot);
    SinTheta := Sin(Theta);

    // is resulting sinus equal to 0? (just to verify, should not happen)
    if (SinTheta = 0.0) then
      raise Exception.Create('Invalid value');

    // calculate the scaling for q1 and q2, according to the angle and it's sine value
    Scale0 := Sin((1.0 - P) * Theta)  / SinTheta;
    Scale1 := Sin((P        * Theta)) / SinTheta;
  end;

  // calculate the resulting quaternion by using a special form of linear interpolation
  Result.FX := (Scale0 * FX) + (Scale1 * InterpolateWith.FX);
  Result.FY := (Scale0 * FY) + (Scale1 * InterpolateWith.FY);
  Result.FZ := (Scale0 * FZ) + (Scale1 * InterpolateWith.FZ);
  Result.FW := (Scale0 * FW) + (Scale1 * InterpolateWith.FW);
end;
//------------------------------------------------------------------------------
function TQRQuaternion.Rotate(const Vector: TQRVector3D): TQRVector3D;
var
  Qv, Qm: TQRQuaternion;
begin
  // rotate vector
  Qv := TQRQuaternion.Create(Vector.FX, Vector.FY, Vector.FZ, 0);
  Qm := Mul(qv.Mul(Inverse));

  Result.FX := Qm.FX;
  Result.FY := Qm.FY;
  Result.FZ := Qm.FZ;
end;
//------------------------------------------------------------------------------
function TQRQuaternion.GetMatrix: TQRMatrix4x4;
begin
  Result := TQRMAtrix4x4.Create(1.0 -  2.0 * (FY * FY + FZ * FZ), 2.0 * (FX *  FY - FW * FZ),       2.0 * (FX *  FZ + FW * FY),       0.0,
                                2.0 * (FX *  FY + FW * FZ),       1.0 -  2.0 * (FX * FX + FZ * FZ), 2.0 * (FY *  FZ - FW * FX),       0.0,
                                2.0 * (FX *  FZ - FW * FY),       2.0 * (FY *  FZ + FW * FX),       1.0 -  2.0 * (FX * FX + FY * FY), 0.0,
                                0.0,                              0.0,                              0.0,                              1.0);
end;
//------------------------------------------------------------------------------
// TQRRay
//------------------------------------------------------------------------------
constructor TQRRay.Create;
var
  Dir: TQRVector3D;
begin
  FPos := Default(TQRVector3D);
  Dir  := Default(TQRVector3D);
  SetDir(@Dir);
end;
//------------------------------------------------------------------------------
constructor TQRRay.Create(const PtOther: TQRRay);
begin
  FPos.Assign(PtOther.FPos);
  FDir.Assign(PtOther.FDir);
  FInvDir.Assign(PtOther.FInvDir);
end;
//------------------------------------------------------------------------------
function TQRRay.GetPos: PQRVector3D;
begin
  Result := @FPos;
end;
//------------------------------------------------------------------------------
procedure TQRRay.SetPos(const PtPos: PQRVector3D);
begin
  FPos := PtPos^;
end;
//------------------------------------------------------------------------------
function TQRRay.GetDir: PQRVector3D;
begin
  Result := @FDir;
end;
//------------------------------------------------------------------------------
procedure TQRRay.SetDir(const PtDir: PQRVector3D);
begin
  FDir.Assign(PtDir^);

  // is x direction empty?
  if (FDir.FX = 0.0) then
    // in this case invert direction on x axis is infinite
    FInvDir.FX := Infinity
  else
    // calculate invert direction on x axis
    FInvDir.FX := (1.0 / FDir.FX);

  // is y direction empty?
  if (FDir.FY = 0.0) then
    // in this case invert direction on y axis is infinite
    FInvDir.FY := Infinity
  else
    // calculate invert direction on y axis
    FInvDir.FY := (1.0 / FDir.FY);

  // is z direction empty?
  if (FDir.FZ = 0.0) then
    // in this case invert direction on z axis is infinite
    FInvDir.FZ := Infinity
  else
    // calculate invert direction on z axis
    FInvDir.FZ := (1.0 / FDir.FZ);
end;
//------------------------------------------------------------------------------
function TQRRay.GetInvDir: PQRVector3D;
begin
  Result := @FInvDir;
end;
//------------------------------------------------------------------------------
// TQRPolygon
//------------------------------------------------------------------------------
function TQRPolygon.IsBetween(const Value, VStart, VEnd: TQRVector3D; Tolerance: Single): Boolean;
begin
  // check if each vector component is between start and end limits
  Result := (IsBetween(Value.X, VStart.X, VEnd.X, Tolerance) and
             IsBetween(Value.Y, VStart.Y, VEnd.Y, Tolerance) and
             IsBetween(Value.Z, VStart.Z, VEnd.Z, Tolerance));
end;
//------------------------------------------------------------------------------
function TQRPolygon.IsBetween(const Value, LStart, LEnd, Tolerance: Single): Boolean;
begin
  // check if each value is between start and end limits considering tolerance
  Result := ((Value >= Min(LStart, LEnd) - Tolerance) and
             (Value <= Max(LStart, LEnd) + Tolerance));
end;
//------------------------------------------------------------------------------
constructor TQRPolygon.Create(const Vertex1, Vertex2, Vertex3: TQRVector3D);
begin
    FVertex[0] := Vertex1;
    FVertex[1] := Vertex2;
    FVertex[2] := Vertex3;
end;
//------------------------------------------------------------------------------
function TQRPolygon.GetVertex(Index: Byte): TQRVector3D;
begin
  // search for index to get
  case (Index) of
    0: Result := FVertex[0];
    1: Result := FVertex[1];
    2: Result := FVertex[2];
  end;
end;
//------------------------------------------------------------------------------
procedure TQRPolygon.SetVertex(Index: Byte; const Vertex: TQRVector3D);
begin
    // search for index to set
    case (Index) of
      0: FVertex[0] := Vertex;
      1: FVertex[1] := Vertex;
      2: FVertex[2] := Vertex;
    end;
end;
//------------------------------------------------------------------------------
function TQRPolygon.GetVertex1: PQRVector3D;
begin
  Result := @FVertex[0];
end;
//------------------------------------------------------------------------------
procedure TQRPolygon.SetVertex1(const PtVertex: PQRVector3D);
begin
  FVertex[0] := PtVertex^;
end;
//------------------------------------------------------------------------------
function TQRPolygon.GetVertex2: PQRVector3D;
begin
  Result := @FVertex[1];
end;
//------------------------------------------------------------------------------
procedure TQRPolygon.SetVertex2(const PtVertex: PQRVector3D);
begin
  FVertex[1] := PtVertex^;
end;
//------------------------------------------------------------------------------
function TQRPolygon.GetVertex3: PQRVector3D;
begin
  Result := @FVertex[2];
end;
//------------------------------------------------------------------------------
procedure TQRPolygon.SetVertex3(const PtVertex: PQRVector3D);
begin
  FVertex[2] := PtVertex^;
end;
//------------------------------------------------------------------------------
function TQRPolygon.GetClone: TQRPolygon;
begin
  // copies the polygon, then returns the copy
  Result := TQRPolygon.Create(FVertex[0], FVertex[1], FVertex[2]);
end;
//------------------------------------------------------------------------------
function TQRPolygon.ApplyMatrix(const Matrix: TQRMatrix4x4): TQRPolygon;
begin
  // build a new polygon transforming all vertices of the polygon using
  // given matrix, and return new built polygon
  Result := TQRPolygon.Create(Matrix.Transform(FVertex[0]),
                              Matrix.Transform(FVertex[1]),
                              Matrix.Transform(FVertex[2]));
end;
//------------------------------------------------------------------------------
function TQRPolygon.GetPlane: TQRPlane;
begin
  // calculates the plane from the values of the 3 vertices of the polygon
  Result := TQRPlane.FromPoints(FVertex[0], FVertex[1], FVertex[2]);
end;
//------------------------------------------------------------------------------
function TQRPolygon.GetCenter: TQRVector3D;
begin
  // calculates then returns the value of the midpoint of the polygon
  Result := TQRVector3D.Create(((FVertex[0].X + FVertex[1].X + FVertex[2].X) / 3.0),
                               ((FVertex[0].Y + FVertex[1].Y + FVertex[2].Y) / 3.0),
                               ((FVertex[0].Z + FVertex[1].Z + FVertex[2].Z) / 3.0));
end;
//------------------------------------------------------------------------------
function TQRPolygon.Inside(const X, Y, Z: Single): Boolean;
begin
  Result := Inside(TQRVector3D.Create(X, Y, Z));
end;
//------------------------------------------------------------------------------
function TQRPolygon.Inside(const Point: TQRVector3D): Boolean;
var
    NPToV1, NPToV2, NPToV3:  TQRVector3D;
    A1, A2, A3, AngleResult: Single;
begin
  { check if the point p is inside the polygon in the following manner:      }
  {                                                                          }
  {                  V1                         V1                           }
  {                  /\                         /\                           }
  {                 /  \                       /  \                          }
  {                / *p \                  *P /    \                         }
  {               /      \                   /      \                        }
  {            V2 -------- V3             V2 -------- V3                     }
  {                                                                          }
  { calculate the vectors between the point p and each polygon vertex, then  }
  { calculate the angle formed by each of these vectors. If the sum of the   }
  { angles is equal to a complete circle, i.e. 2 * pi in radians, then the   }
  { point p is inside the polygon limits, otherwise the point is outside. It }
  { is assumed that the point to check belongs to the polygon's plane        }
  NPToV1 := FVertex[0].Sub(point).Normalize;
  NPToV2 := FVertex[1].Sub(point).Normalize;
  NPToV3 := FVertex[2].Sub(point).Normalize;

  // calculate the angles using the dot product of each vectors. Limit range
  // to values between -1.0f and 1.0f
  A1 := Max(Min(NPToV1.Dot(NPToV2), 1.0), -1.0);
  A2 := Max(Min(NPToV2.Dot(NPToV3), 1.0), -1.0);
  A3 := Max(Min(NPToV3.Dot(NPToV1), 1.0), -1.0);

  // calculate the sum of all angles
  AngleResult := ArcCos(A1) + ArcCos(A2) + ArcCos(A3);

  // if sum is equal to 6.28 radians then point p is inside polygon. NOTE can
  // be higher due to precision errors in calculations
  Result := (AngleResult >= 6.28);
end;
//------------------------------------------------------------------------------
// TQRCircle
//------------------------------------------------------------------------------
function TQRCircle.GetPos: PQRVector2D;
begin
  Result := @FPos;
end;
//------------------------------------------------------------------------------
procedure TQRCircle.SetPos(const PtPos: PQRVector2D);
begin
  FPos := PtPos^;
end;
//------------------------------------------------------------------------------
// TQRSphere
//------------------------------------------------------------------------------
function TQRSphere.GetPos: PQRVector3D;
begin
  Result := @FPos;
end;
//------------------------------------------------------------------------------
procedure TQRSphere.SetPos(const PtPos: PQRVector3D);
begin
  FPos := PtPos^;
end;
//------------------------------------------------------------------------------
// TQRRect
//------------------------------------------------------------------------------
constructor TQRRect.Create(const X, Y, Width, Height: Single);
begin
  FMin := TQRVector2D.Create(X,         Y);
  FMax := TQRVector2D.Create(X + Width, Y + Height);
end;
//------------------------------------------------------------------------------
function TQRRect.GetMin: PQRVector2D;
begin
  Result := @FMin;
end;
//------------------------------------------------------------------------------
procedure TQRRect.SetMin(const PtValue: PQRVector2D);
begin
  FMin := PtValue^;
end;
//------------------------------------------------------------------------------
function TQRRect.GetMax: PQRVector2D;
begin
  Result := @FMax;
end;
//------------------------------------------------------------------------------
procedure TQRRect.SetMax(const PtValue: PQRVector2D);
begin
  FMax := PtValue^;
end;
//------------------------------------------------------------------------------
function TQRRect.GetWidth: Single;
begin
  Result := (FMax.FX - FMin.FX);
end;
//------------------------------------------------------------------------------
function TQRRect.GetHeight: Single;
begin
  Result := (FMax.FY - FMin.FY);
end;
//------------------------------------------------------------------------------
// TQRBox
//------------------------------------------------------------------------------
function TQRBox.GetMin: PQRVector3D;
begin
  Result := @FMin;
end;
//------------------------------------------------------------------------------
procedure TQRBox.SetMin(const PtValue: PQRVector3D);
begin
  FMin := PtValue^;
end;
//------------------------------------------------------------------------------
function TQRBox.GetMax: PQRVector3D;
begin
  Result := @FMax;
end;
//------------------------------------------------------------------------------
procedure TQRBox.SetMax(const PtValue: PQRVector3D);
begin
  FMax := PtValue^;
end;
//------------------------------------------------------------------------------
procedure TQRBox.Cut(var LeftBox: TQRBox; var RightBox: TQRBox);
var
  X, Y, Z:     Single;
  LongestAxis: NativeUInt;
begin
  // calculate each edge length
  X := Abs(FMax.FX - FMin.FX);
  Y := Abs(FMax.FY - FMin.FY);
  Z := Abs(FMax.FZ - FMin.FZ);

  // search for longest axis
  if ((X >= Y) and (X >= Z)) then
    LongestAxis := 0
  else
  if ((Y >= X) and (Y >= Z)) then
    LongestAxis := 1
  else
    LongestAxis := 2;

  // cut box
  case LongestAxis of
    // cut on x axis
    0:
    begin
      LeftBox.FMin.Assign(FMin);
      LeftBox.FMax.Assign(FMax);
      LeftBox.FMax.FX := FMin.FX + (X * 0.5);

      RightBox.FMin.Assign(FMin);
      RightBox.FMax.Assign(FMax);
      RightBox.FMin.FX := LeftBox.FMax.FX;
    end;

    // cut on y axis
    1:
    begin
      LeftBox.FMin.Assign(FMin);
      LeftBox.FMax.Assign(FMax);
      LeftBox.FMax.FY := FMin.FY + (Y * 0.5);

      RightBox.FMin.Assign(FMin);
      RightBox.FMax.Assign(FMax);
      RightBox.FMin.FY := LeftBox.FMax.FY;
    end;

    // cut on z axis
    2:
    begin
      LeftBox.FMin.Assign(FMin);
      LeftBox.FMax.Assign(FMax);
      LeftBox.FMax.FZ := FMin.FZ + (Z * 0.5);

      RightBox.FMin.Assign(FMin);
      RightBox.FMax.Assign(FMax);
      RightBox.FMin.FZ := LeftBox.FMax.FZ;
    end;
  end;
end;
//------------------------------------------------------------------------------

end.
