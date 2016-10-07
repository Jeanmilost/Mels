{**************************************************************************************************
 * ==> UTQRGeometry ------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides all geometry classes, as e.g. vectors and matrix classes    *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRGeometry;

interface

uses System.SysUtils,
     System.Math;

type
    {**
    * Vector 2D
    *}
    TQRVector2D = record
        private
            m_X: Single; // x coordinate for the 2D vector
            m_Y: Single; // y coordinate for the 2D vector

        public
            {**
            * Constructor
            *@param x - vector x coordinate
            *@param y - vector y coordinate
            *}
            constructor Create(const x, y: Single); overload;

            {**
            * Constructor
            *@param other - other vector to copy from
            *}
            constructor Create(const other: TQRVector2D); overload;

            { Basic functions }
            Procedure Assign(const other: TQRVector2D);                              inline;
            function  Add(const value: Single): TQRVector2D;               overload; inline;
            function  Add(const other: TQRVector2D): TQRVector2D;          overload; inline;
            function  Invert: TQRVector2D;                                 overload; inline;
            function  Sub(const value: Single): TQRVector2D;               overload; inline;
            function  Sub(const other: TQRVector2D): TQRVector2D;          overload; inline;
            function  Mul(const value: Single): TQRVector2D;               overload; inline;
            function  Mul(const other: TQRVector2D): TQRVector2D;          overload; inline;
            function  Divide(const value: Single): TQRVector2D;            overload; inline;
            function  Divide(const other: TQRVector2D): TQRVector2D;       overload; inline;
            function  AddAndAssign(const value: Single): TQRVector2D;      overload; inline;
            function  AddAndAssign(const other: TQRVector2D): TQRVector2D; overload; inline;
            function  SubAndAssign(const value: Single): TQRVector2D;      overload; inline;
            function  SubAndAssign(const other: TQRVector2D): TQRVector2D; overload; inline;
            function  MulAndAssign(const value: Single): TQRVector2D;      overload; inline;
            function  MulAndAssign(const other: TQRVector2D): TQRVector2D; overload; inline;
            function  DivAndAssign(const value: Single): TQRVector2D;      overload; inline;
            function  DivAndAssign(const other: TQRVector2D): TQRVector2D; overload; inline;
            function  IsEqual(const other: TQRVector2D): Boolean;                    inline;
            function  Differs(const other: TQRVector2D): Boolean;                    inline;

            {**
            * Gets the vector length
            *@return vector length
            *}
            function Length: Single; inline;

            {**
            * Normalizes the vector
            *@return normalized vector
            *}
            function Normalize: TQRVector2D; inline;

            {**
            * Calculates the cross product between 2 vectors
            *@param vector - vector with which cross product is calculated
            *@return cross product
            *}
            function Cross(const vector: TQRVector2D): TQRVector2D; inline;

            {**
            * Calculates the dot product between 2 vectors
            *@param vector - vector with which dot product is calculated
            *@return dot product
            *}
            function Dot(const vector: TQRVector2D): Single; inline;

            {**
            * Calculates interpolation vector between 2 vectors
            *@param other - other vector to interpolate with
            *@param position - interpolation position, in percent (between 0.0f and 1.0f)
            *@return interpolation vector
            *}
            function Interpolate(const other: TQRVector2D; const position: Single): TQRVector2D; inline;

            { Properties }
            property X: Single read m_X write m_X;
            property Y: Single read m_Y write m_Y;
    end;

    PQRVector2D = ^TQRVector2D;

    {**
    * Vector 3D
    *}
    TQRVector3D = record
        private
            m_X: Single; // x coordinate for the 3D vector
            m_Y: Single; // y coordinate for the 3D vector
            m_Z: Single; // z coordinate for the 3D vector

        public
            {**
            * Constructor
            *@param x - vector x coordinate
            *@param y - vector y coordinate
            *@param z - vector z coordinate
            *}
            constructor Create(const x, y, z: Single); overload;

            {**
            * Constructor
            *@param other - other vector to copy from
            *}
            constructor Create(const other: TQRVector3D); overload;

            { Basic functions }
            procedure Assign(const other: TQRVector3D);                              inline;
            function  Add(const value: Single): TQRVector3D;               overload; inline;
            function  Add(const other: TQRVector3D): TQRVector3D;          overload; inline;
            function  Invert: TQRVector3D;                                 overload; inline;
            function  Sub(const value: Single): TQRVector3D;               overload; inline;
            function  Sub(const other: TQRVector3D): TQRVector3D;          overload; inline;
            function  Mul(const value: Single): TQRVector3D;               overload; inline;
            function  Mul(const other: TQRVector3D): TQRVector3D;          overload; inline;
            function  Divide(const value: Single): TQRVector3D;            overload; inline;
            function  Divide(const other: TQRVector3D): TQRVector3D;       overload; inline;
            function  AddAndAssign(const value: Single): TQRVector3D;      overload; inline;
            function  AddAndAssign(const other: TQRVector3D): TQRVector3D; overload; inline;
            function  SubAndAssign(const value: Single): TQRVector3D;      overload; inline;
            function  SubAndAssign(const other: TQRVector3D): TQRVector3D; overload; inline;
            function  MulAndAssign(const value: Single): TQRVector3D;      overload; inline;
            function  MulAndAssign(const other: TQRVector3D): TQRVector3D; overload; inline;
            function  DivAndAssign(const value: Single): TQRVector3D;      overload; inline;
            function  DivAndAssign(const other: TQRVector3D): TQRVector3D; overload; inline;
            function  IsEqual(const other: TQRVector3D): Boolean;                    inline;
            function  Differs(const other: TQRVector3D): Boolean;                    inline;

            {**
            * Gets the vector length
            *@return vector length
            *}
            function Length: Single; inline;

            {**
            * Normalizes the vector
            *@return normalized vector
            *}
            function Normalize: TQRVector3D; inline;

            {**
            * Calculates the cross product between 2 vectors
            *@param vector - vector with which cross product is calculated
            *@return cross product
            *}
            function Cross(const vector: TQRVector3D): TQRVector3D; inline;

            {**
            * Calculates the dot product between 2 vectors
            *@param vector - vector with which dot product is calculated
            *@return dot product
            *}
            function Dot(const vector: TQRVector3D): Single; inline;

            {**
            * Calculates interpolation vector between 2 vectors
            *@param other - other vector to interpolate with
            *@param position - interpolation position, in percent (between 0.0f and 1.0f)
            *@return interpolation vector
            *}
            function Interpolate(const other: TQRVector3D; const position: Single): TQRVector3D; inline;

            { Properties }
            property X: Single read m_X write m_X;
            property Y: Single read m_Y write m_Y;
            property Z: Single read m_Z write m_Z;
    end;

    PQRVector3D = ^TQRVector3D;

    {**
    * Plane
    *}
    TQRPlane = record
        private
            m_A: Single; // a coordinate for the plane
            m_B: Single; // b coordinate for the plane
            m_C: Single; // c coordinate for the plane
            m_D: Single; // d coordinate for the plane

        public
            {**
            * Constructor
            *@param a - plane a coordinate (relative to formula aX + bY + cZ + d = 0)
            *@param b - plane b coordinate (relative to formula aX + bY + cZ + d = 0)
            *@param c - plane c coordinate (relative to formula aX + bY + cZ + d = 0)
            *@param d - plane d coordinate (relative to formula aX + bY + cZ + d = 0)
            *}
            constructor Create(const a, b, c, d: Single); overload;

            {**
            * Constructor
            *@param other - other plane to copy from
            *}
            constructor Create(const other: TQRPlane); overload;

            { Basic functions }
            procedure Assign(const other: TQRPlane);           inline;
            function  Invert: TQRPlane;                        inline;
            function  IsEqual(const other: TQRPlane): Boolean; inline;
            function  Differs(const other: TQRPlane): Boolean; inline;

            {**
            * Calculates distance to plane
            *@param point - point from which the distance must be calculated
            *@return distance to plane
            *}
            function DistanceTo(const point: TQRVector3D): Single; inline;

            {**
            * Checks if plane intersects line and calculates intersection point
            *@param v1 - line start
            *@param v2 - line end
            *@param[out] p - calculated point on plane
            *@return true if plane intersects line, otherwise false
            *}
            function IntersectLine(const v1, v2: TQRVector3D; out p: TQRVector3D): Boolean; inline;

            {**
            * Checks if plane intersects ray and calculates intersection point
            *@param rp - ray start point
            *@param rd - ray direction
            *@param[out] p - calculated point on plane
            *@return true if plane intersects ray, otherwise false
            *}
            function IntersectRay(const rp, rd: TQRVector3D; out p: TQRVector3D): Boolean; inline;

            {**
            * Compare plane with the given plane using the given tolerance
            *@param other - other plane to compare
            *@param tolerance - tolerance for comparison
            *@return true if planes are equals in the limits of the given tolerance,
            *         otherwise false
            *}
            function Compare(const other: TQRPlane; const tolerance: Single): Boolean; inline;

            {**
            * Calculates a plane using 3 vertex
            *@param v1 - value of the first vertex
            *@param v2 - value of the second vertex
            *@param v3 - value of the thrid vertex
            *@return the built plane
            *}
            class function FromPoints(const v1, v2, v3: TQRVector3D): TQRPlane; static; inline;

            {**
            * Calculates a plane using a point and a normal
            *@param point - a point belongs to the plane
            *@param normal - normal of the plane
            *@return the built plane
            *}
            class function FromPointNormal(const point, normal: TQRVector3D): TQRPlane; static; inline;

            { Properties }
            property A: Single read m_A write m_A;
            property B: Single read m_B write m_B;
            property C: Single read m_C write m_C;
            property D: Single read m_D write m_D;
    end;

    PQRPlane = ^TQRPlane;

    {**
    * 4x4 matrix
    *}
    TQRMatrix4x4 = record
        private
            m_Table: array[0..3] of array [0..3] of Single; // 4x4 matrix array

            {**
            * Gets matrix item at index
            *@param i - index
            *@return item
            *@throw exception if index is out of bounds
            *}
            function  GetItem(i: NativeInt): Single; overload; inline;

            {**
            * Sets matrix item at index
            *@param i - index
            *@param value - value to set
            *@throw exception if index is out of bounds
            *}
            procedure SetItem(i: NativeInt; value: Single); overload; inline;

            {**
            * Gets matrix item from table
            *@param x - table x position
            *@param y - table y position
            *@return item
            *@throw exception if positions are out of bounds
            *}
            function  GetTableItem(x, y: NativeInt): Single; overload; inline;

            {**
            * Sets matrix item to table
            *@param x - table x position
            *@param y - table y position
            *@param value - value to set
            *@throw exception if positions are out of bounds
            *}
            procedure SetTableItem(x, y: NativeInt; value: Single); overload; inline;

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
            *@param other - other matrix to copy from
            *}
            constructor Create(const other: TQRMatrix4x4); overload;

            {**
            * Gets record initialized with default values
            *@return record initialized with default values
            *}
            class function GetDefault: TQRMatrix4x4; inline; static;

            { Basic functions }
            procedure Assign(const other: TQRMatrix4x4);
            function  IsEqual(const other: TQRMatrix4x4): Boolean;
            function  Differs(const other: TQRMatrix4x4): Boolean; inline;

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
            *@return inverse of the matrix
            *}
            function Inverse(out determinant: Single): TQRMatrix4x4;

            {**
            * Multiplies matrix by another matrix
            *@param other - other matrix to multiply with
            *@return multiplied resulting matrix
            *}
            function Multiply(const other: TQRMatrix4x4): TQRMatrix4x4;

            {**
            * Translates matrix
            *@param t - translation vector
            *@return copy of translated matrix
            *}
            function Translate(const t: TQRVector3D): TQRMatrix4x4; inline;

            {**
            * Rotates matrix
            *@param angle - rotation angle in radians
            *@param r - rotation direction (e.g. [0.0f, 0.0f, 1.0f] for a z-axis rotation)
            *@return copy of rotated matrix
            *@note rotation direction vector should be normalized before calling
            *      this function
            *}
            function Rotate(const angle: Single; const r: TQRVector3D): TQRMatrix4x4; inline;

            {**
            * Scales matrix
            *@param s - scale vector
            *@return copy of scaled matrix
            *}
            function Scale(const s: TQRVector3D): TQRMatrix4x4; inline;

            {**
            * Swaps matrix lines and columns
            *@return swapped matrix
            *}
            function Swap: TQRMatrix4x4; inline;

            {**
            * Applies a transformation matrix to a vector
            *@param vector - vector to transform
            *@param matrix - transformation matrix
            *@return transformed vector
            *}
            function Transform(const vector: TQRVector3D): TQRVector3D; inline;

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
            property Item[i: NativeInt]:     Single read GetItem      write SetItem;
            property Table[x, y: NativeInt]: Single read GetTableItem write SetTableItem;
            property Ptr: PSingle                   read GetPtr;
    end;

    PQRMatrix4x4 = ^TQRMatrix4x4;

    {**
    * Quaternion
    *}
    TQRQuaternion = record
        private
            m_X: Single; // x coordinate for the quaternion
            m_Y: Single; // y coordinate for the quaternion
            m_Z: Single; // z coordinate for the quaternion
            m_W: Single; // w coordinate for the quaternion

        public
            {**
            * Constructor
            *@param x - vector x value
            *@param y - vector y value
            *@param z - vector z value
            *@param w - vector w value
            *}
            constructor Create(x, y, z, w: Single); overload;

            {**
            * Constructor that creates a quaternion from an axis and an angle
            *@param vector - vector representing axis
            *@param angle - angle in radians
            *}
            constructor Create(const vector: TQRVector3D; angle: Single); overload;

            {**
            * Constructor that creates a quaternion from a 4x4 matrix
            *@param matrix - matrix
            *}
            constructor Create(const matrix: TQRMatrix4x4); overload;

            {**
            * Copy constructor
            *@param other - other quaternion to copy from
            *}
            constructor Create(const other: TQRQuaternion); overload;

            { Basic functions }
            procedure Assign(const other: TQRQuaternion);                                inline;
            function  Add(const value: Single): TQRQuaternion;                 overload; inline;
            function  Add(const other: TQRQuaternion): TQRQuaternion;          overload; inline;
            function  Invert: TQRQuaternion;                                   overload; inline;
            function  Sub(const value: Single): TQRQuaternion;                 overload; inline;
            function  Sub(const other: TQRQuaternion): TQRQuaternion;          overload; inline;
            function  Mul(const value: Single): TQRQuaternion;                 overload; inline;
            function  Mul(const other: TQRQuaternion): TQRQuaternion;          overload; inline;
            function  Divide(const value: Single): TQRQuaternion;              overload; inline;
            function  Divide(const other: TQRQuaternion): TQRQuaternion;       overload; inline;
            function  AddAndAssign(const value: Single): TQRQuaternion;        overload; inline;
            function  AddAndAssign(const other: TQRQuaternion): TQRQuaternion; overload; inline;
            function  SubAndAssign(const value: Single): TQRQuaternion;        overload; inline;
            function  SubAndAssign(const other: TQRQuaternion): TQRQuaternion; overload; inline;
            function  MulAndAssign(const value: Single): TQRQuaternion;        overload; inline;
            function  MulAndAssign(const other: TQRQuaternion): TQRQuaternion; overload; inline;
            function  DivAndAssign(const value: Single): TQRQuaternion;        overload; inline;
            function  DivAndAssign(const other: TQRQuaternion): TQRQuaternion; overload; inline;
            function  IsEqual(const other: TQRQuaternion): Boolean;                      inline;
            function  Differs(const other: TQRQuaternion): Boolean;                      inline;

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
            *@param q - quaternion with which dot product is calculated
            *@return dot product
            *}
            function Dot(const q: TQRQuaternion): Single; inline;

            {**
            * Scales the quaternion
            *@param s - scale factor to apply
            *@return scaled quaternion
            *}
            function Scale(s: Single): TQRQuaternion; inline;

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
            *@param other - other quaternion to interpolate with
            *@param p - interpolation position, in percent (between 0.0f and 1.0f)
            *@return the spherical linear interpolated quaternion
            *}
            function Slerp(const other: TQRQuaternion; p: Single): TQRQuaternion; inline;

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
            property X: Single read m_X write m_X;
            property Y: Single read m_Y write m_Y;
            property Z: Single read m_Z write m_Z;
            property W: Single read m_W write m_W;
    end;

    PQRQuaternion = ^TQRQuaternion;

    {**
    * Euclidean ray
    *}
    TQRRay = class
        protected
            m_Pos:    TQRVector3D;
            m_Dir:    TQRVector3D;
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
            *@param pOther - other ray to copy from
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
            *@param value - value to check
            *@param vStart - start limit
            *@param vEnd - end limit
            *@param tolerance - tolerance
            *@return true if value is between limits, otherwise false
            *}
            function IsBetween(const value, vStart, vEnd: TQRVector3D;
                                               tolerance: Single): Boolean; overload;

            {**
            * Checks if a value is between start and end limits
            *@param value - value to check
            *@param lStart - start limit
            *@param lEnd - end limit
            *@param tolerance - tolerance
            *@return true if value is between limits, otherwise false
            *}
            function IsBetween(const value, lStart, lEnd, tolerance: Single): Boolean; overload;

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
            *@param vertex - vertex value
            *}
            procedure SetVertex(index: Byte; const vertex: TQRVector3D);

            {**
            * Gets first polygon vertex
            *@return first polygon vertex
            *}
            function GetVertex1: PQRVector3D;

            {**
            * Sets first polygon vertex
            *@param pVertex - first polygon vertex value
            *}
            procedure SetVertex1(const pVertex: PQRVector3D);

            {**
            * Gets second polygon vertex
            *@return second polygon vertex
            *}
            function GetVertex2: PQRVector3D;

            {**
            * Sets second polygon vertex
            *@param pVertex - second polygon vertex value
            *}
            procedure SetVertex2(const pVertex: PQRVector3D);

            {**
            * Gets third polygon vertex
            *@return third polygon vertex
            *}
            function GetVertex3: PQRVector3D;

            {**
            * Sets third polygon vertex
            *@param pVertex - third polygon vertex value
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
            function Inside(const x, y, z: Single): Boolean; overload;

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
            m_Pos:    TQRVector2D;
            m_Radius: Single;

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
            property Radius: Single      read m_Radius write m_Radius;
    end;

    {**
    * 3D sphere
    *}
    TQRSphere = record
        private
            m_Pos:    TQRVector3D;
            m_Radius: Single;

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
            property Radius: Single      read m_Radius write m_Radius;
    end;

    {**
    * 2D rectangle
    *}
    TQRRect = record
        private
            m_Min: TQRVector2D;
            m_Max: TQRVector2D;

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
            constructor Create(const x, y, width, height: Single);

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
            m_Min: TQRVector3D;
            m_Max: TQRVector3D;

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
//--------------------------------------------------------------------------------------------------
// TQRVector2D
//--------------------------------------------------------------------------------------------------
constructor TQRVector2D.Create(const x, y: Single);
begin
    m_X := x;
    m_Y := y;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRVector2D.Create(const other: TQRVector2D);
begin
    Assign(other);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVector2D.Assign(const other: TQRVector2D);
begin
    m_X := other.m_X;
    m_Y := other.m_Y;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Add(const value: Single): TQRVector2D;
begin
    result := TQRVector2D.Create(m_X + value, m_Y + value);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Add(const other: TQRVector2D): TQRVector2D;
begin
    result := TQRVector2D.Create(m_X + other.m_X, m_Y + other.m_Y);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Invert: TQRVector2D;
begin
    result := TQRVector2D.Create(-m_X, -m_Y);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Sub(const value: Single): TQRVector2D;
begin
    Result := TQRVector2D.Create(m_X - value, m_Y - value);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Sub(const other: TQRVector2D): TQRVector2D;
begin
    Result := TQRVector2D.Create(m_X - other.m_X, m_Y - other.m_Y);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Mul(const value: Single): TQRVector2D;
begin
    Result := TQRVector2D.Create(m_X * value, m_Y * value);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Mul(const other: TQRVector2D): TQRVector2D;
begin
    Result := TQRVector2D.Create(m_X * other.m_X, m_Y * other.m_Y);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Divide(const value: Single): TQRVector2D;
begin
    if (value = 0.0) then
        raise Exception.Create('Division by 0 is prohibited');

    Result := TQRVector2D.Create(m_X / value, m_Y / value);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Divide(const other: TQRVector2D): TQRVector2D;
begin
    if (other.m_X = 0.0) then
        raise Exception.Create('Vector x axis - division by 0 is prohibited');

    if (other.m_Y = 0.0) then
        raise Exception.Create('Vector y axis - division by 0 is prohibited');

    Result := TQRVector2D.Create(m_X / other.m_X, m_Y / other.m_Y);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.AddAndAssign(const value: Single): TQRVector2D;
begin
    m_X := m_X + value;
    m_Y := m_Y + value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.AddAndAssign(const other: TQRVector2D): TQRVector2D;
begin
    m_X := m_X + other.m_X;
    m_Y := m_Y + other.m_Y;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.SubAndAssign(const value: Single): TQRVector2D;
begin
    m_X := m_X - value;
    m_Y := m_Y - value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.SubAndAssign(const other: TQRVector2D): TQRVector2D;
begin
    m_X := m_X - other.m_X;
    m_Y := m_Y - other.m_Y;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.MulAndAssign(const value: Single): TQRVector2D;
begin
    m_X := m_X * value;
    m_Y := m_Y * value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.MulAndAssign(const other: TQRVector2D): TQRVector2D;
begin
    m_X := m_X * other.m_X;
    m_Y := m_Y * other.m_Y;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.DivAndAssign(const value: Single): TQRVector2D;
begin
    if (value = 0.0) then
        raise Exception.Create('Division by 0 is prohibited');

    m_X := m_X / value;
    m_Y := m_Y / value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.DivAndAssign(const other: TQRVector2D): TQRVector2D;
begin
    if (other.m_X = 0.0) then
        raise Exception.Create('Vector x axis - division by 0 is prohibited');

    if (other.m_Y = 0.0) then
        raise Exception.Create('Vector y axis - division by 0 is prohibited');

    m_X := m_X / other.m_X;
    m_Y := m_Y / other.m_Y;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.IsEqual(const other: TQRVector2D): Boolean;
begin
    Result := ((m_X = other.m_X) and (m_Y = other.m_Y));
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Differs(const other: TQRVector2D): Boolean;
begin
    Result := ((m_X <> other.m_X) or (m_Y <> other.m_Y));
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Length: Single;
begin
    Result := sqrt((m_X * m_X) + (m_Y * m_Y));
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Normalize: TQRVector2D;
var
    len: Single;
begin
    len := Length;

    if (len = 0.0) then
        Result := Default(TQRVector2D)
    else
        Result := TQRVector2D.Create((m_X / len), (m_Y / len));
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Cross(const vector: TQRVector2D): TQRVector2D;
begin
    Result := TQRVector2D.Create((m_Y * vector.m_X) - (vector.m_Y * m_X),
                                 (m_X * vector.m_Y) - (vector.m_X * m_Y));
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Dot(const vector: TQRVector2D): Single;
begin
    Result := ((m_X * vector.m_X) + (m_Y * vector.m_Y));
end;
//--------------------------------------------------------------------------------------------------
function TQRVector2D.Interpolate(const other: TQRVector2D; const position: Single): TQRVector2D;
begin
    // is position out of bounds? Limit to min or max values in this case
    if (position < 0.0) then
    begin
        Result := Self;
        Exit;
    end
    else
    if (position > 1.0) then
    begin
        Result := other;
        Exit;
    end;

    // calculate interpolation
    Result.m_X := m_X + position * (other.m_X - m_X);
    Result.m_Y := m_Y + position * (other.m_Y - m_Y);
end;
//--------------------------------------------------------------------------------------------------
// TQRVector3D
//--------------------------------------------------------------------------------------------------
constructor TQRVector3D.Create(const x, y, z: Single);
begin
    m_X := x;
    m_Y := y;
    m_Z := z;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRVector3D.Create(const other: TQRVector3D);
begin
    Assign(other);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVector3D.Assign(const other: TQRVector3D);
begin
    m_X := other.m_X;
    m_Y := other.m_Y;
    m_Z := other.m_Z;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Add(const value: Single): TQRVector3D;
begin
    Result := TQRVector3D.Create(m_X + value, m_Y + value, m_Z + value);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Add(const other: TQRVector3D): TQRVector3D;
begin
    Result := TQRVector3D.Create(m_X + other.m_X, m_Y + other.m_Y, m_Z + other.m_Z);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Invert: TQRVector3D;
begin
    Result := TQRVector3D.Create(-m_X, -m_Y, -m_Z);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Sub(const value: Single): TQRVector3D;
begin
    Result := TQRVector3D.Create(m_X - value, m_Y - value, m_Z - value);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Sub(const other: TQRVector3D): TQRVector3D;
begin
    Result := TQRVector3D.Create(m_X - other.m_X, m_Y - other.m_Y, m_Z - other.m_Z);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Mul(const value: Single): TQRVector3D;
begin
    Result := TQRVector3D.Create(m_X * value, m_Y * value, m_Z * value);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Mul(const other: TQRVector3D): TQRVector3D;
begin
    Result := TQRVector3D.Create(m_X * other.m_X, m_Y * other.m_Y, m_Z * other.m_Z);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Divide(const value: Single): TQRVector3D;
begin
    if (value = 0.0) then
        raise Exception.Create('Division by 0 is prohibited');

    Result := TQRVector3D.Create(m_X / value, m_Y / value, m_Z / value);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Divide(const other: TQRVector3D): TQRVector3D;
begin
    if (other.m_X = 0.0) then
        raise Exception.Create('Vector x axis - division by 0 is prohibited');

    if (other.m_Y = 0.0) then
        raise Exception.Create('Vector y axis - division by 0 is prohibited');

    if (other.m_Z = 0.0) then
        raise Exception.Create('Vector z axis - division by 0 is prohibited');

    Result := TQRVector3D.Create(m_X / other.m_X, m_Y / other.m_Y, m_Z / other.m_Z);
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.AddAndAssign(const value: Single): TQRVector3D;
begin
    m_X := m_X + value;
    m_Y := m_Y + value;
    m_Z := m_Z + value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.AddAndAssign(const other: TQRVector3D): TQRVector3D;
begin
    m_X := m_X + other.m_X;
    m_Y := m_Y + other.m_Y;
    m_Z := m_Z + other.m_Z;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.SubAndAssign(const value: Single): TQRVector3D;
begin
    m_X := m_X - value;
    m_Y := m_Y - value;
    m_Z := m_Z - value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.SubAndAssign(const other: TQRVector3D): TQRVector3D;
begin
    m_X := m_X - other.m_X;
    m_Y := m_Y - other.m_Y;
    m_Z := m_Z - other.m_Z;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.MulAndAssign(const value: Single): TQRVector3D;
begin
    m_X := m_X * value;
    m_Y := m_Y * value;
    m_Z := m_Z * value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.MulAndAssign(const other: TQRVector3D): TQRVector3D;
begin
    m_X := m_X * other.m_X;
    m_Y := m_Y * other.m_Y;
    m_Z := m_Z * other.m_Z;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.DivAndAssign(const value: Single): TQRVector3D;
begin
    if (value = 0.0) then
        raise Exception.Create('Division by 0 is prohibited');

    m_X := m_X / value;
    m_Y := m_Y / value;
    m_Z := m_Z / value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.DivAndAssign(const other: TQRVector3D): TQRVector3D;
begin
    if (other.m_X = 0.0) then
        raise Exception.Create('Vector x axis - division by 0 is prohibited');

    if (other.m_Y = 0.0) then
        raise Exception.Create('Vector y axis - division by 0 is prohibited');

    if (other.m_Z = 0.0) then
        raise Exception.Create('Vector z axis - division by 0 is prohibited');

    m_X := m_X / other.m_X;
    m_Y := m_Y / other.m_Y;
    m_Z := m_Z / other.m_Z;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.IsEqual(const other: TQRVector3D): Boolean;
begin
    Result := ((m_X = other.m_X) and (m_Y = other.m_Y) and (m_Z = other.m_Z));
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Differs(const other: TQRVector3D): Boolean;
begin
    Result := ((m_X <> other.m_X) or (m_Y <> other.m_Y) or (m_Z <> other.m_Z));
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Length: Single;
begin
    Result := sqrt((m_X * m_X) + (m_Y * m_Y) + (m_Z * m_Z));
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Normalize: TQRVector3D;
var
    len: Single;
begin
    len := Length;

    if (len = 0.0) then
        Result := default(TQRVector3D)
    else
        Result := TQRVector3D.Create((m_X / len), (m_Y / len), (m_Z / len));
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Cross(const vector: TQRVector3D): TQRVector3D;
begin
    Result := TQRVector3D.Create((m_Y * vector.m_Z) - (vector.m_Y * m_Z),
                                 (m_Z * vector.m_X) - (vector.m_Z * m_X),
                                 (m_X * vector.m_Y) - (vector.m_X * m_Y));
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Dot(const vector: TQRVector3D): Single;
begin
    Result := ((m_X * vector.m_X) + (m_Y * vector.m_Y) + (m_Z * vector.m_Z));
end;
//--------------------------------------------------------------------------------------------------
function TQRVector3D.Interpolate(const other: TQRVector3D; const position: Single): TQRVector3D;
begin
    // is position out of bounds? Limit to min or max values in this case
    if (position < 0.0) then
    begin
        Result := Self;
        Exit;
    end
    else
    if (position > 1.0) then
    begin
        Result := other;
        Exit;
    end;

    // calculate interpolation
    Result.m_X := m_X + position * (other.m_X - m_X);
    Result.m_Y := m_Y + position * (other.m_Y - m_Y);
    Result.m_Z := m_Z + position * (other.m_Z - m_Z);
end;
//--------------------------------------------------------------------------------------------------
// TQRPlane
//--------------------------------------------------------------------------------------------------
constructor TQRPlane.Create(const a, b, c, d: Single);
begin
    m_A := a;
    m_B := b;
    m_C := c;
    m_D := d;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRPlane.Create(const other: TQRPlane);
begin
    Assign(other);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPlane.Assign(const other: TQRPlane);
begin
    m_A := other.m_A;
    m_B := other.m_B;
    m_C := other.m_C;
    m_D := other.m_D;
end;
//--------------------------------------------------------------------------------------------------
function TQRPlane.Invert: TQRPlane;
begin
    Result := TQRPlane.Create(-m_A, -m_B, -m_C, -m_D);
end;
//--------------------------------------------------------------------------------------------------
function TQRPlane.IsEqual(const other: TQRPlane): Boolean;
begin
    Result := ((m_A = other.m_A) and (m_B = other.m_B) and (m_C = other.m_C) and (m_D = other.m_D));
end;
//--------------------------------------------------------------------------------------------------
function TQRPlane.Differs(const other: TQRPlane): Boolean;
begin
    Result := ((m_A <> other.m_A) or (m_B <> other.m_B) or (m_C <> other.m_C) or (m_D <> other.m_D));
end;
//--------------------------------------------------------------------------------------------------
function TQRPlane.DistanceTo(const point: TQRVector3D): Single;
var
    n: TQRVector3D;
begin
    // get the normal of the plane
    n := TQRVector3D.Create(m_A, m_B, m_C);

    // calculate the distance between the plane and the point
    Result := n.Dot(point) + m_D;
end;
//--------------------------------------------------------------------------------------------------
function TQRPlane.IntersectLine(const v1, v2: TQRVector3D; out p: TQRVector3D): Boolean;
var
    direction: TQRVector3D;
begin
    // calculates the direction of the line
    direction := v2.Sub(v1);

    Result := IntersectRay(v1, direction.Normalize, p);
end;
//--------------------------------------------------------------------------------------------------
function TQRPlane.IntersectRay(const rp, rd: TQRVector3D; out p: TQRVector3D): Boolean;
var
    normal:    TQRVector3D;
    dot, temp: Single;
begin
    // gets the normal of the plane
    normal := TQRVector3D.Create(m_A, m_B, m_C);

    // calculates the angle between the line and the normal to the plane
    dot := normal.Dot(rd);

    // if normal to the plane is perpendicular to the line, then the line is
    // either parallel to the plane and there are no solutions or the line is
    // on the plane in which case there are an infinite number of solutions
    if (dot = 0.0) then
    begin
        Result := False;
        Exit;
    end;

    temp := (m_D + normal.Dot(rp)) / dot;

    // calculates the intersection point
    p := TQRVector3D.Create(rp.m_X - (temp * rd.m_X),
                            rp.m_Y - (temp * rd.m_Y),
                            rp.m_Z - (temp * rd.m_Z));

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRPlane.Compare(const other: TQRPlane; const tolerance: Single): Boolean;
begin
    Result := (((m_A >= (other.m_A - tolerance))  and
                (m_A <= (other.m_A + tolerance))) and
               ((m_B >= (other.m_B - tolerance))  and
                (m_B <= (other.m_B + tolerance))) and
               ((m_C >= (other.m_C - tolerance))  and
                (m_C <= (other.m_C + tolerance))) and
               ((m_D >= (other.m_D - tolerance))  and
                (m_D <= (other.m_D + tolerance))));
end;
//--------------------------------------------------------------------------------------------------
class function TQRPlane.FromPoints(const v1, v2, v3: TQRVector3D): TQRPlane;
var
    e1, e2, normal: TQRVector3D;
begin
    // calculate edge vectors
    e1 := v2.Sub(v1);
    e2 := v3.Sub(v1);

    // calculate the normal of the plane
    normal := e1.Cross(e2).Normalize;

    // calculate and return the plane
    Result := FromPointNormal(v1, normal);
end;
//--------------------------------------------------------------------------------------------------
class function TQRPlane.FromPointNormal(const point, normal: TQRVector3D): TQRPlane;
begin
    // the a, b, and c components are only the normal of the plane, and the D
    // component can be calculated using the aX + bY + cZ + d = 0 algorithm
    Result := TQRPlane.Create(normal.m_X, normal.m_Y, normal.m_Z, -(normal.Dot(point)));
end;
//--------------------------------------------------------------------------------------------------
// TQRMatrix4x4
//--------------------------------------------------------------------------------------------------
constructor TQRMatrix4x4.Create(const _11, _12, _13, _14,
                                      _21, _22, _23, _24,
                                      _31, _32, _33, _34,
                                      _41, _42, _43, _44: Single);
begin
    m_Table[0][0] := _11; m_Table[0][1] := _12; m_Table[0][2] := _13; m_Table[0][3] := _14;
    m_Table[1][0] := _21; m_Table[1][1] := _22; m_Table[1][2] := _23; m_Table[1][3] := _24;
    m_Table[2][0] := _31; m_Table[2][1] := _32; m_Table[2][2] := _33; m_Table[2][3] := _34;
    m_Table[3][0] := _41; m_Table[3][1] := _42; m_Table[3][2] := _43; m_Table[3][3] := _44;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRMatrix4x4.Create(const other: TQRMatrix4x4);
begin
    Assign(other);
end;
//--------------------------------------------------------------------------------------------------
class function TQRMatrix4x4.GetDefault: TQRMatrix4x4;
begin
    Result := Identity;
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.GetItem(i: NativeInt): Single;
var
    x, y: NativeInt;
begin
    if (i > 15) then
        raise Exception.Create('Index is out of bounds');

    // calculate x and y position from index
    x := i mod 4;
    y := i div 4;

    Result := m_Table[x][y];
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMatrix4x4.SetItem(i: NativeInt; value: Single);
var
    x, y: NativeInt;
begin
    if (i > 15) then
        raise Exception.Create('Index is out of bounds');

    // calculate x and y position from index
    x := i mod 4;
    y := i div 4;

    m_Table[x][y] := value;
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.GetTableItem(x, y: NativeInt): Single;
begin
    if (x > 3) then
        raise Exception.Create('X index is out of bounds');

    if (y > 3) then
        raise Exception.Create('Y index is out of bounds');

    Result := m_Table[x][y];
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMatrix4x4.SetTableItem(x, y: NativeInt; value: Single);
begin
    if (x > 3) then
        raise Exception.Create('X index is out of bounds');

    if (y > 3) then
        raise Exception.Create('Y index is out of bounds');

    m_Table[x][y] := value;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMatrix4x4.Assign(const other: TQRMatrix4x4);
var
    i, j: Byte;
begin
    // copy matrix table from other
    for i := 0 to 3 do
        for j := 0 to 3 do
            m_Table[i][j] := other.m_Table[i][j];
end;
//--------------------------------------------------------------------------------------------------
// FIXME MUST NOT BE INLINE!!!
function TQRMatrix4x4.IsEqual(const other: TQRMatrix4x4): Boolean;
var
    i, j: Byte;
begin
    // compare each matrix element with other matrix
    for i := 0 to 3 do
        for j := 0 to 3 do
            if (m_Table[i][j] <> other.m_Table[i][j]) then
            begin
                Result := False;
                Exit;
            end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Differs(const other: TQRMatrix4x4): Boolean;
begin
    Result := not IsEqual(other);
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.IsIdentity: Boolean;
begin
    Result := IsEqual(Identity);
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Determinant: Single;
var
    t: array [0..2] of Single;
    v: array [0..3] of Single;
begin
    t[0] := m_Table[2][2] * m_Table[3][3] - m_Table[2][3] * m_Table[3][2];
    t[1] := m_Table[1][2] * m_Table[3][3] - m_Table[1][3] * m_Table[3][2];
    t[2] := m_Table[1][2] * m_Table[2][3] - m_Table[1][3] * m_Table[2][2];

    v[0] :=  m_Table[1][1] * t[0] - m_Table[2][1] * t[1] + m_Table[3][1] * t[2];
    v[1] := -m_Table[1][0] * t[0] + m_Table[2][0] * t[1] - m_Table[3][0] * t[2];

    t[0] := m_Table[1][0] * m_Table[2][1] - m_Table[2][0] * m_Table[1][1];
    t[1] := m_Table[1][0] * m_Table[3][1] - m_Table[3][0] * m_Table[1][1];
    t[2] := m_Table[2][0] * m_Table[3][1] - m_Table[3][0] * m_Table[2][1];

    v[2] :=  m_Table[3][3] * t[0] - m_Table[2][3] * t[1] + m_Table[1][3] * t[2];
    v[3] := -m_Table[3][2] * t[0] + m_Table[2][2] * t[1] - m_Table[1][2] * t[2];

    Result := m_Table[0][0] * v[0] +
              m_Table[0][1] * v[1] +
              m_Table[0][2] * v[2] +
              m_Table[0][3] * v[3];
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Inverse(out determinant: Single): TQRMatrix4x4;
var
    invDet: Single;
    t:      array [0..2]  of Single;
    v:      array [0..15] of Single;
    i, j:   NativeUInt;
begin
    t[0] := m_Table[2][2] * m_Table[3][3] - m_Table[2][3] * m_Table[3][2];
    t[1] := m_Table[1][2] * m_Table[3][3] - m_Table[1][3] * m_Table[3][2];
    t[2] := m_Table[1][2] * m_Table[2][3] - m_Table[1][3] * m_Table[2][2];

    v[0] :=  m_Table[1][1] * t[0] - m_Table[2][1] * t[1] + m_Table[3][1] * t[2];
    v[4] := -m_Table[1][0] * t[0] + m_Table[2][0] * t[1] - m_Table[3][0] * t[2];

    t[0] :=  m_Table[1][0] * m_Table[2][1] - m_Table[2][0] * m_Table[1][1];
    t[1] :=  m_Table[1][0] * m_Table[3][1] - m_Table[3][0] * m_Table[1][1];
    t[2] :=  m_Table[2][0] * m_Table[3][1] - m_Table[3][0] * m_Table[2][1];

    v[8]  :=  m_Table[3][3] * t[0] - m_Table[2][3] * t[1] + m_Table[1][3] * t[2];
    v[12] := -m_Table[3][2] * t[0] + m_Table[2][2] * t[1] - m_Table[1][2] * t[2];

    determinant := m_Table[0][0] * v[0] +
                   m_Table[0][1] * v[4] +
                   m_Table[0][2] * v[8] +
                   m_Table[0][3] * v[12];

    if (determinant = 0.0) then
        Exit;

    t[0] := m_Table[2][2] * m_Table[3][3] - m_Table[2][3] * m_Table[3][2];
    t[1] := m_Table[0][2] * m_Table[3][3] - m_Table[0][3] * m_Table[3][2];
    t[2] := m_Table[0][2] * m_Table[2][3] - m_Table[0][3] * m_Table[2][2];

    v[1] := -m_Table[0][1] * t[0] + m_Table[2][1] * t[1] - m_Table[3][1] * t[2];
    v[5] :=  m_Table[0][0] * t[0] - m_Table[2][0] * t[1] + m_Table[3][0] * t[2];

    t[0] := m_Table[0][0] * m_Table[2][1] - m_Table[2][0] * m_Table[0][1];
    t[1] := m_Table[3][0] * m_Table[0][1] - m_Table[0][0] * m_Table[3][1];
    t[2] := m_Table[2][0] * m_Table[3][1] - m_Table[3][0] * m_Table[2][1];

    v[9]  := -m_Table[3][3] * t[0] - m_Table[2][3] * t[1] - m_Table[0][3] * t[2];
    v[13] :=  m_Table[3][2] * t[0] + m_Table[2][2] * t[1] + m_Table[0][2] * t[2];

    t[0] := m_Table[1][2] * m_Table[3][3] - m_Table[1][3] * m_Table[3][2];
    t[1] := m_Table[0][2] * m_Table[3][3] - m_Table[0][3] * m_Table[3][2];
    t[2] := m_Table[0][2] * m_Table[1][3] - m_Table[0][3] * m_Table[1][2];

    v[2] :=  m_Table[0][1] * t[0] - m_Table[1][1] * t[1] + m_Table[3][1] * t[2];
    v[6] := -m_Table[0][0] * t[0] + m_Table[1][0] * t[1] - m_Table[3][0] * t[2];

    t[0] := m_Table[0][0] * m_Table[1][1] - m_Table[1][0] * m_Table[0][1];
    t[1] := m_Table[3][0] * m_Table[0][1] - m_Table[0][0] * m_Table[3][1];
    t[2] := m_Table[1][0] * m_Table[3][1] - m_Table[3][0] * m_Table[1][1];

    v[10] :=  m_Table[3][3] * t[0] + m_Table[1][3] * t[1] + m_Table[0][3] * t[2];
    v[14] := -m_Table[3][2] * t[0] - m_Table[1][2] * t[1] - m_Table[0][2] * t[2];

    t[0] := m_Table[1][2] * m_Table[2][3] - m_Table[1][3] * m_Table[2][2];
    t[1] := m_Table[0][2] * m_Table[2][3] - m_Table[0][3] * m_Table[2][2];
    t[2] := m_Table[0][2] * m_Table[1][3] - m_Table[0][3] * m_Table[1][2];

    v[3] := -m_Table[0][1] * t[0] + m_Table[1][1] * t[1] - m_Table[2][1] * t[2];
    v[7] :=  m_Table[0][0] * t[0] - m_Table[1][0] * t[1] + m_Table[2][0] * t[2];

    v[11] := -m_Table[0][0] * (m_Table[1][1] * m_Table[2][3] - m_Table[1][3] * m_Table[2][1]) +
              m_Table[1][0] * (m_Table[0][1] * m_Table[2][3] - m_Table[0][3] * m_Table[2][1]) -
              m_Table[2][0] * (m_Table[0][1] * m_Table[1][3] - m_Table[0][3] * m_Table[1][1]);

    v[15] := m_Table[0][0] * (m_Table[1][1] * m_Table[2][2] - m_Table[1][2] * m_Table[2][1]) -
             m_Table[1][0] * (m_Table[0][1] * m_Table[2][2] - m_Table[0][2] * m_Table[2][1]) +
             m_Table[2][0] * (m_Table[0][1] * m_Table[1][2] - m_Table[0][2] * m_Table[1][1]);

    invDet := 1.0 / determinant;

    for i := 0 to 3 do
        for j := 0 to 3 do
            Result.m_Table[i][j] := v[4 * i + j] * invDet;
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Multiply(const other: TQRMatrix4x4): TQRMatrix4x4;
var
    i, j: Byte;
begin
    for i := 0 to 3 do
        for j := 0 to 3 do
            Result.m_Table[i][j] := m_Table[i][0] * other.m_Table[0][j] +
                                    m_Table[i][1] * other.m_Table[1][j] +
                                    m_Table[i][2] * other.m_Table[2][j] +
                                    m_Table[i][3] * other.m_Table[3][j];
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Translate(const t: TQRVector3D): TQRMatrix4x4;
begin
    m_Table[3][0] := m_Table[3][0] + (m_Table[0][0] * t.m_X + m_Table[1][0] * t.m_Y + m_Table[2][0] * t.m_Z);
    m_Table[3][1] := m_Table[3][1] + (m_Table[0][1] * t.m_X + m_Table[1][1] * t.m_Y + m_Table[2][1] * t.m_Z);
    m_Table[3][2] := m_Table[3][2] + (m_Table[0][2] * t.m_X + m_Table[1][2] * t.m_Y + m_Table[2][2] * t.m_Z);
    m_Table[3][3] := m_Table[3][3] + (m_Table[0][3] * t.m_X + m_Table[1][3] * t.m_Y + m_Table[2][3] * t.m_Z);

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Rotate(const angle: Single; const r: TQRVector3D): TQRMatrix4x4;
var
    c, s, ic: Single;
    matrix:   TQRMatrix4x4;
begin
    // calculate sinus, cosinus and inverted cosinus values
    c  := Cos(angle);
    s  := Sin(angle);
    ic := (1.0 - c);

    // create rotation matrix
    matrix               := Identity;
    matrix.m_Table[0][0] := (ic * r.m_X * r.m_X) + c;
    matrix.m_Table[1][0] := (ic * r.m_X * r.m_Y) - (s * r.m_Z);
    matrix.m_Table[2][0] := (ic * r.m_X * r.m_Z) + (s * r.m_Y);
    matrix.m_Table[0][1] := (ic * r.m_Y * r.m_X) + (s * r.m_Z);
    matrix.m_Table[1][1] := (ic * r.m_Y * r.m_Y) + c;
    matrix.m_Table[2][1] := (ic * r.m_Y * r.m_Z) - (s * r.m_X);
    matrix.m_Table[0][2] := (ic * r.m_Z * r.m_X) - (s * r.m_Y);
    matrix.m_Table[1][2] := (ic * r.m_Z * r.m_Y) + (s * r.m_X);
    matrix.m_Table[2][2] := (ic * r.m_Z * r.m_Z) + c;

    // combine current matrix with rotation matrix
    Self := matrix.Multiply(Self);

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Scale(const s: TQRVector3D): TQRMatrix4x4;
begin
    m_Table[0][0] := m_Table[0][0] * s.m_X; m_Table[1][0] := m_Table[1][0] * s.m_Y; m_Table[2][0] := m_Table[2][0] * s.m_Z;
    m_Table[0][1] := m_Table[0][1] * s.m_X; m_Table[1][1] := m_Table[1][1] * s.m_Y; m_Table[2][1] := m_Table[2][1] * s.m_Z;
    m_Table[0][2] := m_Table[0][2] * s.m_X; m_Table[1][2] := m_Table[1][2] * s.m_Y; m_Table[2][2] := m_Table[2][2] * s.m_Z;
    m_Table[0][3] := m_Table[0][3] * s.m_X; m_Table[1][3] := m_Table[1][3] * s.m_Y; m_Table[2][3] := m_Table[2][3] * s.m_Z;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Swap: TQRMatrix4x4;
begin
    Result := TQRMatrix4x4.Create(m_Table[0][0], m_Table[1][0], m_Table[2][0], m_Table[3][0],
                                  m_Table[0][1], m_Table[1][1], m_Table[2][1], m_Table[3][1],
                                  m_Table[0][2], m_Table[1][2], m_Table[2][2], m_Table[3][2],
                                  m_Table[0][3], m_Table[1][3], m_Table[2][3], m_Table[3][3]);
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.Transform(const vector: TQRVector3D): TQRVector3D;
begin
    // calculates x, y and z coordinates (don't use w component), and returns
    // transformed vector
    Result := TQRVector3D.Create((vector.m_X * m_Table[0][0] +
                                  vector.m_Y * m_Table[1][0] +
                                  vector.m_Z * m_Table[2][0] +
                                  m_Table[3][0]),
                                 (vector.m_X * m_Table[0][1] +
                                  vector.m_Y * m_Table[1][1] +
                                  vector.m_Z * m_Table[2][1] +
                                  m_Table[3][1]),
                                 (vector.m_X * m_Table[0][2] +
                                  vector.m_Y * m_Table[1][2] +
                                  vector.m_Z * m_Table[2][2] +
                                  m_Table[3][2]));
end;
//--------------------------------------------------------------------------------------------------
function TQRMatrix4x4.GetPtr: PSingle;
begin
    Result := @m_Table[0][0];
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
constructor TQRQuaternion.Create(x, y, z, w: Single);
begin
    m_X := x;
    m_Y := y;
    m_Z := z;
    m_W := w;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRQuaternion.Create(const vector: TQRVector3D; angle: Single);
var
    sinAngle: Single;
begin
    sinAngle := Sin(angle);

    m_X := (vector.m_X * sinAngle);
    m_Y := (vector.m_Y * sinAngle);
    m_Z := (vector.m_Z * sinAngle);
    m_W :=  Cos(angle);
end;
//--------------------------------------------------------------------------------------------------
constructor TQRQuaternion.Create(const matrix: TQRMatrix4x4);
var
    diagonal, scale: Single;
begin
    // calculate the matrix diagonal by adding up it's diagonal indices (also known as "trace")
    diagonal := matrix.m_Table[0][0] +
                matrix.m_Table[1][1] +
                matrix.m_Table[2][2] +
                matrix.m_Table[3][3];

    // is diagonal greater than zero?
    if (diagonal > 0.00000001) then
    begin
        // calculate the diagonal scale
        scale := Sqrt(diagonal) * 2.0;

        // calculate the quaternion values using the respective equation
        m_X := (matrix.m_Table[1][2] - matrix.m_Table[2][1]) / scale;
        m_Y := (matrix.m_Table[2][0] - matrix.m_Table[0][2]) / scale;
        m_Z := (matrix.m_Table[0][1] - matrix.m_Table[1][0]) / scale;
        m_W := 0.25 * scale;

        Exit;
    end;

    // search for highest value in the matrix diagonal
    if ((matrix.m_Table[0][0] > matrix.m_Table[1][1]) and (matrix.m_Table[0][0] > matrix.m_Table[2][2])) then
    begin
        // calculate scale using the first diagonal element and double that value
        scale := Sqrt(1.0 + matrix.m_Table[0][0] - matrix.m_Table[1][1] - matrix.m_Table[2][2]) * 2.0;

        // calculate the quaternion values using the respective equation
        m_X := 0.25 * scale;
        m_Y := (matrix.m_Table[0][1] + matrix.m_Table[1][0]) / scale;
        m_Z := (matrix.m_Table[2][0] + matrix.m_Table[0][2]) / scale;
        m_W := (matrix.m_Table[1][2] - matrix.m_Table[2][1]) / scale;
    end
    else
    if (matrix.m_Table[1][1] > matrix.m_Table[2][2]) then
    begin
        // calculate scale using the second diagonal element and double that value
        scale := Sqrt(1.0 + matrix.m_Table[1][1] - matrix.m_Table[0][0] - matrix.m_Table[2][2]) * 2.0;

        // calculate the quaternion values using the respective equation
        m_X := (matrix.m_Table[0][1] + matrix.m_Table[1][0]) / scale;
        m_Y := 0.25 * scale;
        m_Z := (matrix.m_Table[1][2] + matrix.m_Table[2][1]) / scale;
        m_W := (matrix.m_Table[2][0] - matrix.m_Table[0][2]) / scale;
    end
    else
    begin
        // calculate scale using the third diagonal element and double that value
        scale := Sqrt(1.0 + matrix.m_Table[2][2] - matrix.m_Table[0][0] - matrix.m_Table[1][1]) * 2.0;

        // calculate the quaternion values using the respective equation
        m_X := (matrix.m_Table[2][0] + matrix.m_Table[0][2]) / scale;
        m_Y := (matrix.m_Table[1][2] + matrix.m_Table[2][1]) / scale;
        m_Z := 0.25 * scale;
        m_W := (matrix.m_Table[0][1] - matrix.m_Table[1][0]) / scale;
    end
end;
//--------------------------------------------------------------------------------------------------
constructor TQRQuaternion.Create(const other: TQRQuaternion);
begin
    Assign(other);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRQuaternion.Assign(const other: TQRQuaternion);
begin
    m_X := other.m_X;
    m_Y := other.m_Y;
    m_Z := other.m_Z;
    m_W := other.m_W;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Add(const value: Single): TQRQuaternion;
begin
    Result := TQRQuaternion.Create(m_X + value, m_Y + value, m_Z + value, m_W + value);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Add(const other: TQRQuaternion): TQRQuaternion;
begin
    Result := TQRQuaternion.Create(m_X + other.m_X,
                                   m_Y + other.m_Y,
                                   m_Z + other.m_Z,
                                   m_W + other.m_W);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Invert: TQRQuaternion;
begin
    Result := TQRQuaternion.Create(-m_X, -m_Y, -m_Z, -m_W);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Sub(const value: Single): TQRQuaternion;
begin
    Result := TQRQuaternion.Create(m_X - value, m_Y - value, m_Z - value, m_W - value);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Sub(const other: TQRQuaternion): TQRQuaternion;
begin
    Result := TQRQuaternion.Create(m_X - other.m_X,
                                   m_Y - other.m_Y,
                                   m_Z - other.m_Z,
                                   m_W - other.m_W);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Mul(const value: Single): TQRQuaternion;
begin
    Result := TQRQuaternion.Create(m_X * value, m_Y * value, m_Z * value, m_W * value);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Mul(const other: TQRQuaternion): TQRQuaternion;
begin
    Result := TQRQuaternion.Create(m_X * other.m_X,
                                   m_Y * other.m_Y,
                                   m_Z * other.m_Z,
                                   m_W * other.m_W);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Divide(const value: Single): TQRQuaternion;
begin
    if (value = 0.0) then
        raise Exception.Create('Division by 0 is prohibited');

    Result := TQRQuaternion.Create(m_X / value, m_Y / value, m_Z / value, m_W / value);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Divide(const other: TQRQuaternion): TQRQuaternion;
begin
    if (other.m_X = 0.0) then
        raise Exception.Create('Quaternion x value - division by 0 is prohibited');

    if (other.m_Y = 0.0) then
        raise Exception.Create('Quaternion y value - division by 0 is prohibited');

    if (other.m_Z = 0.0) then
        raise Exception.Create('Quaternion z value - division by 0 is prohibited');

    if (other.m_W = 0.0) then
        raise Exception.Create('Quaternion z value - division by 0 is prohibited');

    Result := TQRQuaternion.Create(m_X / other.m_X,
                                   m_Y / other.m_Y,
                                   m_Z / other.m_Z,
                                   m_W / other.m_W);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.AddAndAssign(const value: Single): TQRQuaternion;
begin
    m_X := m_X + value;
    m_Y := m_Y + value;
    m_Z := m_Z + value;
    m_W := m_W + value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.AddAndAssign(const other: TQRQuaternion): TQRQuaternion;
begin
    m_X := m_X + other.m_X;
    m_Y := m_Y + other.m_Y;
    m_Z := m_Z + other.m_Z;
    m_W := m_W + other.m_W;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.SubAndAssign(const value: Single): TQRQuaternion;
begin
    m_X := m_X - value;
    m_Y := m_Y - value;
    m_Z := m_Z - value;
    m_W := m_W - value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.SubAndAssign(const other: TQRQuaternion): TQRQuaternion;
begin
    m_X := m_X - other.m_X;
    m_Y := m_Y - other.m_Y;
    m_Z := m_Z - other.m_Z;
    m_W := m_W - other.m_W;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.MulAndAssign(const value: Single): TQRQuaternion;
begin
    m_X := m_X * value;
    m_Y := m_Y * value;
    m_Z := m_Z * value;
    m_W := m_W * value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.MulAndAssign(const other: TQRQuaternion): TQRQuaternion;
begin
    m_X := m_X * other.m_X;
    m_Y := m_Y * other.m_Y;
    m_Z := m_Z * other.m_Z;
    m_W := m_W * other.m_W;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.DivAndAssign(const value: Single): TQRQuaternion;
begin
    if (value = 0.0) then
        raise Exception.Create('Division by 0 is prohibited');

    m_X := m_X / value;
    m_Y := m_Y / value;
    m_Z := m_Z / value;
    m_W := m_W / value;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.DivAndAssign(const other: TQRQuaternion): TQRQuaternion;
begin
    if (other.m_X = 0.0) then
        raise Exception.Create('Quaternion x value - division by 0 is prohibited');

    if (other.m_Y = 0.0) then
        raise Exception.Create('Quaternion y value - division by 0 is prohibited');

    if (other.m_Z = 0.0) then
        raise Exception.Create('Quaternion z value - division by 0 is prohibited');

    if (other.m_W = 0.0) then
        raise Exception.Create('Quaternion w value - division by 0 is prohibited');

    m_X := m_X / other.m_X;
    m_Y := m_Y / other.m_Y;
    m_Z := m_Z / other.m_Z;
    m_W := m_W / other.m_W;

    Result := Self;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.IsEqual(const other: TQRQuaternion): Boolean;
begin
    Result := ((m_X = other.m_X) and (m_Y = other.m_Y) and (m_Z = other.m_Z) and (m_W = other.m_W));
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Differs(const other: TQRQuaternion): Boolean;
begin
    Result := ((m_X <> other.m_X) or (m_Y <> other.m_Y) or (m_Z <> other.m_Z) or (m_W <> other.m_W));
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Norm: Single;
begin
    Result := ((m_X * m_X) + (m_Y * m_Y) + (m_Z * m_Z) + (m_W * m_W));
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Length: Single;
begin
    Result := Sqrt(Norm);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Normalize: TQRQuaternion;
var
    len: Single;
begin
    len := Length;

    if (len = 0.0) then
    begin
        Result := Default(TQRQuaternion);
        Exit;
    end;

    Result := TQRQuaternion.Create((m_X / len), (m_Y / len), (m_Z / len), (m_W / len));
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Dot(const q: TQRQuaternion): Single;
begin
    Result := ((m_X * q.m_X) + (m_Y * q.m_Y) + (m_Z * q.m_Z) + (m_W * q.m_W));
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Scale(s: Single): TQRQuaternion;
begin
    Result := TQRQuaternion.Create(m_X * s, m_Y * s, m_Z * s, m_W * s);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Conjugate: TQRQuaternion;
begin
    Result := TQRQuaternion.Create(-m_X, -m_Y, -m_Z, m_W);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Inverse: TQRQuaternion;
var
    quatNorm: Single;
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
function TQRQuaternion.Slerp(const other: TQRQuaternion; p: Single): TQRQuaternion;
var
    quatDot, scale0, scale1, theta, sinTheta: Single;
    interpolateWith:                          TQRQuaternion;
begin
    // are quaternions identical?
    if (IsEqual(other)) then
    begin
        Result := Self;
        Exit;
    end;

    // calculate dot product between q1 and q2
    quatDot := Dot(other);

    // check if angle is higher than 90° (this happen if dot product is less than 0)
    if (quatDot < 0.0) then
    begin
        // negate the second quaternion and the dot product result
        interpolateWith :=  other.Invert;
        quatDot         := -quatDot;
    end
    else
        interpolateWith := other;

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
            raise Exception.Create('Invalid value');

        // calculate the scaling for q1 and q2, according to the angle and it's sine value
        scale0 := Sin((1.0 - p) * theta)  / sinTheta;
        scale1 := Sin((p        * theta)) / sinTheta;
    end;

    // calculate the resulting quaternion by using a special form of linear interpolation
    Result.m_X := (scale0 * m_X) + (scale1 * interpolateWith.m_X);
    Result.m_Y := (scale0 * m_Y) + (scale1 * interpolateWith.m_Y);
    Result.m_Z := (scale0 * m_Z) + (scale1 * interpolateWith.m_Z);
    Result.m_W := (scale0 * m_W) + (scale1 * interpolateWith.m_W);
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.Rotate(const vector: TQRVector3D): TQRVector3D;
var
    qv, qm: TQRQuaternion;
begin
    // rotate vector
    qv := TQRQuaternion.Create(vector.m_X, vector.m_Y, vector.m_Z, 0);
    qm := Mul(qv.Mul(Inverse));

    Result.m_X := qm.m_X;
    Result.m_Y := qm.m_Y;
    Result.m_Z := qm.m_Z;
end;
//--------------------------------------------------------------------------------------------------
function TQRQuaternion.GetMatrix: TQRMatrix4x4;
begin
    Result := TQRMAtrix4x4.Create(1.0 -  2.0 * (m_Y * m_Y + m_Z * m_Z), 2.0 * (m_X *  m_Y - m_W * m_Z),       2.0 * (m_X *  m_Z + m_W * m_Y),       0.0,
                                  2.0 * (m_X *  m_Y + m_W * m_Z),       1.0 -  2.0 * (m_X * m_X + m_Z * m_Z), 2.0 * (m_Y *  m_Z - m_W * m_X),       0.0,
                                  2.0 * (m_X *  m_Z - m_W * m_Y),       2.0 * (m_Y *  m_Z + m_W * m_X),       1.0 -  2.0 * (m_X * m_X + m_Y * m_Y), 0.0,
                                  0.0,                                  0.0,                                  0.0,                                  1.0);
end;
//--------------------------------------------------------------------------------------------------
// TQRRay
//--------------------------------------------------------------------------------------------------
constructor TQRRay.Create;
var
    dir: TQRVector3D;
begin
    m_Pos := Default(TQRVector3D);
    dir   := Default(TQRVector3D);
    SetDir(@dir);
end;
//--------------------------------------------------------------------------------------------------
constructor TQRRay.Create(const pOther: TQRRay);
begin
    m_Pos.Assign(pOther.m_Pos);
    m_Dir.Assign(pOther.m_Dir);
    m_InvDir.Assign(pOther.m_InvDir);
end;
//--------------------------------------------------------------------------------------------------
function TQRRay.GetPos: PQRVector3D;
begin
    Result := @m_Pos;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRRay.SetPos(const pPos: PQRVector3D);
begin
    m_Pos := pPos^;
end;
//--------------------------------------------------------------------------------------------------
function TQRRay.GetDir: PQRVector3D;
begin
    Result := @m_Dir;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRRay.SetDir(const pDir: PQRVector3D);
begin
    m_Dir.Assign(pDir^);

    // is x direction empty?
    if (m_Dir.m_X = 0.0) then
        // in this case invert direction on x axis is infinite
        m_InvDir.m_X := Infinity
    else
        // calculate invert direction on x axis
        m_InvDir.m_X := (1.0 / m_Dir.m_X);

    // is y direction empty?
    if (m_Dir.m_Y = 0.0) then
        // in this case invert direction on y axis is infinite
        m_InvDir.m_Y := Infinity
    else
        // calculate invert direction on y axis
        m_InvDir.m_Y := (1.0 / m_Dir.m_Y);

    // is z direction empty?
    if (m_Dir.m_Z = 0.0) then
        // in this case invert direction on z axis is infinite
        m_InvDir.m_Z := Infinity
    else
        // calculate invert direction on z axis
        m_InvDir.m_Z := (1.0 / m_Dir.m_Z);
end;
//--------------------------------------------------------------------------------------------------
function TQRRay.GetInvDir: PQRVector3D;
begin
    Result := @m_InvDir;
end;
//--------------------------------------------------------------------------------------------------
// TQRPolygon
//--------------------------------------------------------------------------------------------------
function TQRPolygon.IsBetween(const value, vStart, vEnd: TQRVector3D; tolerance: Single): Boolean;
begin
    // check if each vector component is between start and end limits
    Result := (IsBetween(value.X, vStart.X, vEnd.X, tolerance) and
               IsBetween(value.Y, vStart.Y, vEnd.Y, tolerance) and
               IsBetween(value.Z, vStart.Z, vEnd.Z, tolerance));
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.IsBetween(const value, lStart, lEnd, tolerance: Single): Boolean;
begin
    // check if each value is between start and end limits considering tolerance
    Result := ((value >= Min(lStart, lEnd) - tolerance) and
               (value <= Max(lStart, lEnd) + tolerance));
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
        0: Result := m_Vertex[0];
        1: Result := m_Vertex[1];
        2: Result := m_Vertex[2];
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPolygon.SetVertex(index: Byte; const vertex: TQRVector3D);
begin
    // search for index to set
    case (index) of
        0: m_Vertex[0] := vertex;
        1: m_Vertex[1] := vertex;
        2: m_Vertex[2] := vertex;
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
    // calculates the plane from the values of the 3 vertices of the polygon
    Result := TQRPlane.FromPoints(m_Vertex[0], m_Vertex[1], m_Vertex[2]);
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.GetCenter: TQRVector3D;
begin
    // calculates then returns the value of the midpoint of the polygon
    Result := TQRVector3D.Create(((m_Vertex[0].X + m_Vertex[1].X + m_Vertex[2].X) / 3.0),
                                 ((m_Vertex[0].Y + m_Vertex[1].Y + m_Vertex[2].Y) / 3.0),
                                 ((m_Vertex[0].Z + m_Vertex[1].Z + m_Vertex[2].Z) / 3.0));
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.Inside(const x, y, z: Single): Boolean;
begin
    Result := Inside(TQRVector3D.Create(x, y, z));
end;
//--------------------------------------------------------------------------------------------------
function TQRPolygon.Inside(const point: TQRVector3D): Boolean;
var
    nPToV1, nPToV2, nPToV3:  TQRVector3D;
    a1, a2, a3, angleResult: Single;
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
    nPToV1 := m_Vertex[0].Sub(point).Normalize;
    nPToV2 := m_Vertex[1].Sub(point).Normalize;
    nPToV3 := m_Vertex[2].Sub(point).Normalize;

    // calculate the angles using the dot product of each vectors. Limit range
    // to values between -1.0f and 1.0f
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
    Result := @m_Pos;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRCircle.SetPos(const pPos: PQRVector2D);
begin
    m_Pos := pPos^;
end;
//--------------------------------------------------------------------------------------------------
// TQRSphere
//--------------------------------------------------------------------------------------------------
function TQRSphere.GetPos: PQRVector3D;
begin
    Result := @m_Pos;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRSphere.SetPos(const pPos: PQRVector3D);
begin
    m_Pos := pPos^;
end;
//--------------------------------------------------------------------------------------------------
// TQRRect
//--------------------------------------------------------------------------------------------------
constructor TQRRect.Create(const x, y, width, height: Single);
begin
    m_Min := TQRVector2D.Create(x,         y);
    m_Max := TQRVector2D.Create(x + width, y + height);
end;
//--------------------------------------------------------------------------------------------------
function TQRRect.GetMin: PQRVector2D;
begin
    Result := @m_Min;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRRect.SetMin(const pValue: PQRVector2D);
begin
    m_Min := pValue^;
end;
//--------------------------------------------------------------------------------------------------
function TQRRect.GetMax: PQRVector2D;
begin
    Result := @m_Max;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRRect.SetMax(const pValue: PQRVector2D);
begin
    m_Max := pValue^;
end;
//--------------------------------------------------------------------------------------------------
function TQRRect.GetWidth: Single;
begin
    Result := (m_Max.m_X - m_Min.m_X);
end;
//--------------------------------------------------------------------------------------------------
function TQRRect.GetHeight: Single;
begin
    Result := (m_Max.m_Y - m_Min.m_Y);
end;
//--------------------------------------------------------------------------------------------------
// TQRBox
//--------------------------------------------------------------------------------------------------
function TQRBox.GetMin: PQRVector3D;
begin
    Result := @m_Min;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRBox.SetMin(const pValue: PQRVector3D);
begin
    m_Min := pValue^;
end;
//--------------------------------------------------------------------------------------------------
function TQRBox.GetMax: PQRVector3D;
begin
    Result := @m_Max;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRBox.SetMax(const pValue: PQRVector3D);
begin
    m_Max := pValue^;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRBox.Cut(var leftBox: TQRBox; var rightBox: TQRBox);
var
    x, y, z:     Single;
    longestAxis: NativeUInt;
begin
    // calculate each edge length
    x := Abs(m_Max.m_X - m_Min.m_X);
    y := Abs(m_Max.m_Y - m_Min.m_Y);
    z := Abs(m_Max.m_Z - m_Min.m_Z);

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
            leftBox.m_Min.Assign(m_Min);
            leftBox.m_Max.Assign(m_Max);
            leftBox.m_Max.m_X := m_Min.m_X + (x * 0.5);

            rightBox.m_Min.Assign(m_Min);
            rightBox.m_Max.Assign(m_Max);
            rightBox.m_Min.m_X := leftBox.m_Max.m_X;
        end;

        // cut on y axis
        1:
        begin
            leftBox.m_Min.Assign(m_Min);
            leftBox.m_Max.Assign(m_Max);
            leftBox.m_Max.m_Y := m_Min.m_Y + (y * 0.5);

            rightBox.m_Min.Assign(m_Min);
            rightBox.m_Max.Assign(m_Max);
            rightBox.m_Min.m_Y := leftBox.m_Max.m_Y;
        end;

        // cut on z axis
        2:
        begin
            leftBox.m_Min.Assign(m_Min);
            leftBox.m_Max.Assign(m_Max);
            leftBox.m_Max.m_Z := m_Min.m_Z + (z * 0.5);

            rightBox.m_Min.Assign(m_Min);
            rightBox.m_Max.Assign(m_Max);
            rightBox.m_Min.m_Z := leftBox.m_Max.m_Z;
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------

end.
