// *************************************************************************************************
// * ==> UTQRCollisions ---------------------------------------------------------------------------*
// *************************************************************************************************
// * MIT License - The Mels Library, a free and easy-to-use 3D Models library                      *
// *                                                                                               *
// * Permission is hereby granted, free of charge, to any person obtaining a copy of this software *
// * and associated documentation files (the "Software"), to deal in the Software without          *
// * restriction, including without limitation the rights to use, copy, modify, merge, publish,    *
// * distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the *
// * Software is furnished to do so, subject to the following conditions:                          *
// *                                                                                               *
// * The above copyright notice and this permission notice shall be included in all copies or      *
// * substantial portions of the Software.                                                         *
// *                                                                                               *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING *
// * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND    *
// * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,  *
// * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      *
// * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *
// *************************************************************************************************

{**
 @abstract(@name provides the classes and fuctions for collision detection.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRCollision;

interface

uses System.Math,
     UTQRCommon,
     UTQRGeometry,
     UTQR3D;

const
    {$REGION 'Documentation'}
    {**
     Epsilon value used for tolerance
    }
    {$ENDREGION}
    QR_Epsilon = 1.0E-3;

type
    {$REGION 'Documentation'}
    {**
     Aligned-axis bounding box tree node pointer (needed to be declared before record itself)
    }
    {$ENDREGION}
    PQRAABBNode = ^TQRAABBNode;

    {$REGION 'Documentation'}
    {**
     Aligned-axis bounding box tree node
    }
    {$ENDREGION}
    TQRAABBNode = record
        {$REGION 'Documentation'}
        {**
         Parent of this node in the AABB tree
        }
        {$ENDREGION}
        m_pParent: PQRAABBNode;

        {$REGION 'Documentation'}
        {**
         Left child node, contains the collision data that were detected inside the left child box
         after the division
        }
        {$ENDREGION}
        m_pLeft: PQRAABBNode;

        {$REGION 'Documentation'}
        {**
         Right child node, contains the collision data that were detected inside the right child box
         after the division
        }
        {$ENDREGION}
        m_pRight: PQRAABBNode;

        {$REGION 'Documentation'}
        {**
         Bounding box matching with this node
        }
        {$ENDREGION}
        m_pBox: PQRBox;

        {$REGION 'Documentation'}
        {**
         Subdivided polygons that the bounding box surrounds
        }
        {$ENDREGION}
        m_Polygons: TQRPolygons;
    end;

    {$REGION 'Documentation'}
    {**
     Aligned-axis bounding box tree
    }
    {$ENDREGION}
    TQRAABBTree = class
        private
            m_pRoot: PQRAABBNode;

        protected
            {$REGION 'Documentation'}
            {**
             Releases tree content
             @param(pNode Root node from which content should be released)
            }
            {$ENDREGION}
            procedure Release(pNode: PQRAABBNode); virtual;

            {$REGION 'Documentation'}
            {**
             Checks if a value is between 2 values
             @param(value Value to test)
             @param(valueStart Start value)
             @param(valueEnd End value)
             @param(epsilon Epsilon value for tolerance)
             @return(@true if value is between values, otherwise @false)
            }
            {$ENDREGION}
            function ValueIsBetween(const value, valueStart, valueEnd, epsilon: Single): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Checks if a point is between 2 vertices
             @param(point Point to test)
             @param(pointStart Start vertex)
             @param(pointEnd End vertex)
             @param(epsilon Epsilon value for tolerance)
             @return(@true if value is between points, otherwise @false)
            }
            {$ENDREGION}
            function VectorIsBetween(const point, pointStart, pointEnd: TQRVector3D;
                                                         const epsilon: Single): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Adds a polygon inside an existing bounding box
             @param(polygon Polygon to add)
             @param(box Bounding box in which polygon should be added)
             @param(empty @bold([in, out]) If @true, box is empty an still no contains any polygon)
            }
            {$ENDREGION}
            procedure AddPolygonToBoundingBox(polygon: TQRPolygon;
                                                 pBox: PQRBox;
                                            var empty: Boolean); virtual;

            {$REGION 'Documentation'}
            {**
             Populates AABB tree
             @param(pNode Root or parent node to create from)
             @param(polygons Source polygon array)
             @param(hIsCanceled Callback function that allows to break the operation, can be @nil)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Populate(pNode: PQRAABBNode;
                     const polygons: TQRPolygons;
                        hIsCanceled: TQRIsCanceledEvent): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Resolves AABB tree
             @param(pRay Ray against which tree boxes will be tested)
             @param(pNode Root or parent node to resolve)
             @param(polygons @bold([in, out]) Polygons belonging to boxes hit by ray)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Polygon list content should be deleted when useless
            }
            {$ENDREGION}
            function Resolve(const pRay: TQRRay;
                            const pNode: PQRAABBNode;
                           var polygons: TQRPolygons): Boolean; overload; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Populates AABB tree
             @param(polygons Source polygon array)
             @param(hIsCanceled Callback function that allows to break the operation, can be @nil)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Populate(const polygons: TQRPolygons;
                                 hIsCanceled: TQRIsCanceledEvent = nil): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Resolves AABB tree
             @param(pRay Ray against which tree boxes will be tested)
             @param(polygons @bold([in, out]) Polygons belonging to boxes hit by ray)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Polygon list content should be deleted when useless
            }
            {$ENDREGION}
            function Resolve(const pRay: TQRRay;
                           var polygons: TQRPolygons): Boolean; overload; virtual;
    end;

    {$REGION 'Documentation'}
    {**
     3D collision detection helper
    }
    {$ENDREGION}
    TQRCollisionHelper = class
        protected
            {$REGION 'Documentation'}
            {**
             Adds polygon to array
             @param(vb Source vertex buffer)
             @param(v1 First polygon vertex index in vertex buffer)
             @param(v2 Second polygon vertex index in vertex buffer)
             @param(v3 Third polygon vertex index in vertex buffer)
             @param(polygons @bold([in, out]) Polygon array that contains generated polygons)
             @br @bold(NOTE) Generated polygons should be deleted when useless
            }
            {$ENDREGION}
            class procedure AddPolygon(const vb: TQRVertexBuffer;
                                     v1, v2, v3: NativeUInt;
                                   var polygons: TQRPolygons); static;

        public
            {$REGION 'Documentation'}
            {**
             Tests collision between a ray and a polygon
             @param(pRay Ray)
             @param(polygon Polygon to check)
             @return(@true if ray intersects polygon, otherwise @false)
            }
            {$ENDREGION}
            class function GetRayPolygonCollision(const pRay: TQRRay;
                                               const polygon: TQRPolygon): Boolean; static;

            {$REGION 'Documentation'}
            {**
             Tests collision between a ray and a box
             @param(pRay Ray)
             @param(pBox Box)
             @return(@true if ray intersects box, otherwise @false)
            }
            {$ENDREGION}
            class function GetRayBoxCollision(const pRay: TQRRay; const pBox: PQRBox): Boolean; static;

            {$REGION 'Documentation'}
            {**
             Gets polygons from vertex
             @param(vertex Source vertex descriptor, contains buffer to get from)
             @param(polygons @bold([in, out]) Polygon list to populate)
             @param(hIsCanceled Callback function that allows to break the operation, can be @nil)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Polygon list content should be deleted when useless
            }
            {$ENDREGION}
            class function GetPolygons(const vertex: TQRVertex;
                                       var polygons: TQRPolygons;
                                        hIsCanceled: TQRIsCanceledEvent = nil): Boolean; static;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRAABBTree
//--------------------------------------------------------------------------------------------------
constructor TQRAABBTree.Create;
begin
    inherited Create;

    m_pRoot := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor  TQRAABBTree.Destroy;
begin
    // delete entire tree hierarchy
    Release(m_pRoot);

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRAABBTree.Release(pNode: PQRAABBNode);
begin
    // nothing to do?
    if (not Assigned(pNode)) then
        Exit;

    // release all children on left side
    if (Assigned(pNode.m_pLeft)) then
        Release(pNode.m_pLeft);

    // release all children on right side
    if (Assigned(pNode.m_pRight)) then
        Release(pNode.m_pRight);

    // delete aabb box
    if (Assigned(pNode.m_pBox)) then
        Dispose(pNode.m_pBox);

    // delete polygons
    SetLength(pNode.m_Polygons, 0);

    // delete node
    Dispose(pNode);
end;
//--------------------------------------------------------------------------------------------------
function TQRAABBTree.ValueIsBetween(const value, valueStart, valueEnd, epsilon: Single): Boolean;
var
    minVal, maxVal: Single;
begin
    minVal := Min(valueStart, valueEnd);
    maxVal := Max(valueStart, valueEnd);

    // check if each value is between start and end limits considering tolerance
    Result := ((value >= (minVal - epsilon)) and (value <= (maxVal + epsilon)));
end;
//--------------------------------------------------------------------------------------------------
function TQRAABBTree.VectorIsBetween(const point, pointStart, pointEnd: TQRVector3D;
                                                         const epsilon: Single): Boolean;
begin
    // check if each vector component is between start and end limits
    Result := (ValueIsBetween(point.X, pointStart.X, pointEnd.X, epsilon) and
               ValueIsBetween(point.Y, pointStart.Y, pointEnd.Y, epsilon) and
               ValueIsBetween(point.Z, pointStart.Z, pointEnd.Z, epsilon));
end;
//--------------------------------------------------------------------------------------------------
procedure TQRAABBTree.AddPolygonToBoundingBox(polygon: TQRPolygon;
                                                 pBox: PQRBox;
                                            var empty: Boolean);
var
    i: Byte;
begin
    // no box to add to
    if (not Assigned(pBox)) then
        Exit;

    // iterate through polygon vertices
    for i := 0 to 2 do
    begin
        // is box empty?
        if (empty) then
        begin
            // initialize bounding box with first vertex
            pBox.Min.Assign(polygon.GetVertex(i));
            pBox.Max.Assign(polygon.GetVertex(i));
            empty := False;
            continue;
        end;

        // search for box min edge
        pBox.Min.Assign(TQRVector3D.Create(Min(pBox.Min.X, polygon.GetVertex(i).X),
                                           Min(pBox.Min.Y, polygon.GetVertex(i).Y),
                                           Min(pBox.Min.Z, polygon.GetVertex(i).Z)));

        // search for box max edge
        pBox.Max.Assign(TQRVector3D.Create(Max(pBox.Max.X, polygon.GetVertex(i).X),
                                           Max(pBox.Max.Y, polygon.GetVertex(i).Y),
                                           Max(pBox.Max.Z, polygon.GetVertex(i).Z)));
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRAABBTree.Populate(pNode: PQRAABBNode;
                     const polygons: TQRPolygons;
                        hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    polygonIndex,
    leftPolygonIndex,
    rightPolygonIndex,
    polygonCount,
    leftPolygonCount,
    rightPolygonCount:                         NativeUInt;
    polygon:                                   TQRPolygon;
    i, j:                                      Byte;
    pLeftBox, pRightBox:                       PQRBox;
    leftPolygons, rightPolygons:               TQRPolygons;
    boxEmpty, canResolveLeft, canResolveRight: Boolean;
begin
    boxEmpty := True;
    Result   := False;

    // initialize node content
    pNode.m_pLeft  := nil;
    pNode.m_pRight := nil;
    pNode.m_pBox   := nil;
    SetLength(pNode.m_Polygons, 0);

    // is canceled?
    if (Assigned(hIsCanceled) and hIsCanceled) then
        Exit;

    // create a collision box
    New(pNode.m_pBox);

    // iterate through polygons to divide
    for polygon in polygons do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
            Exit;

        // calculate bounding box
        AddPolygonToBoundingBox(polygon, pNode.m_pBox, boxEmpty);
    end;

    pLeftBox  := nil;
    pRightBox := nil;

    try
        // create left and right boxes
        New(pLeftBox);
        New(pRightBox);

        // divide box in 2 sub-boxes
        pNode.m_pBox.Cut(pLeftBox^, pRightBox^);

        // iterate again through polygons to divide
        for polygon in polygons do
            for i := 0 to 2 do
            begin
                // is canceled?
                if (Assigned(hIsCanceled) and hIsCanceled) then
                    Exit;

                // check if first polygon vertice belongs to left or right sub-box
                if (VectorIsBetween(polygon.GetVertex(i),
                                    pLeftBox.Min^,
                                    pLeftBox.Max^,
                                    QR_Epsilon))
                then
                begin
                    // get current left polygon index
                    leftPolygonIndex := Length(leftPolygons);

                    // add polygon to left list
                    SetLength(leftPolygons, leftPolygonIndex + 1);

                    // copy polygon
                    for j := 0 to 2 do
                        leftPolygons[leftPolygonIndex].SetVertex(j, polygon.GetVertex(j));

                    break;
                end
                else
                if (VectorIsBetween(polygon.GetVertex(i),
                                    pRightBox.Min^,
                                    pRightBox.Max^,
                                    QR_Epsilon))
                then
                begin
                    // get current right polygon index
                    rightPolygonIndex := Length(rightPolygons);

                    // add polygon to right list
                    SetLength(rightPolygons, rightPolygonIndex + 1);

                    // copy polygon
                    for j := 0 to 2 do
                        rightPolygons[rightPolygonIndex].SetVertex(j, polygon.GetVertex(j));

                    break;
                end;
            end;
    finally
        if (Assigned(pLeftBox)) then
            Dispose(pLeftBox);

        if (Assigned(pRightBox)) then
            Dispose(pRightBox);
    end;

    polygonCount      := Length(polygons);
    leftPolygonCount  := Length(leftPolygons);
    rightPolygonCount := Length(rightPolygons);
    canResolveLeft    := ((leftPolygonCount  > 0) and (leftPolygonCount  < polygonCount));
    canResolveRight   := ((rightPolygonCount > 0) and (rightPolygonCount < polygonCount));

    // leaf reached?
    if ((not canResolveLeft) and (not canResolveRight)) then
    begin
        // iterate through polygons to copy
        for polygon in polygons do
        begin
            // get current polygon index
            polygonIndex := Length(pNode.m_Polygons);

            // add polygon to leaf
            SetLength(pNode.m_Polygons, polygonIndex + 1);

            // copy polygon content
            for i := 0 to 2 do
                pNode.m_Polygons[polygonIndex].SetVertex(i, polygon.GetVertex(i));
        end;

        // delete left and right lists, as they will no more be used
        SetLength(leftPolygons,  0);
        SetLength(rightPolygons, 0);

        Exit(True);
    end;

    // do create left node?
    if (canResolveLeft) then
    begin
        // create and populate left node
        New(pNode.m_pLeft);
        pNode.m_pLeft.m_pParent := pNode;

        Result := Populate(pNode.m_pLeft, leftPolygons, hIsCanceled) or Result;

        // delete current list, as it will no more be used
        SetLength(leftPolygons, 0);
    end;

    // do create right node?
    if (canResolveRight) then
    begin
        // create and populate right node
        New(pNode.m_pRight);
        pNode.m_pRight.m_pParent := pNode;

        Result := Populate(pNode.m_pRight, rightPolygons, hIsCanceled) or Result;

        // delete current list, as it will no more be used
        SetLength(rightPolygons, 0);
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRAABBTree.Resolve(const pRay: TQRRay;
                            const pNode: PQRAABBNode;
                           var polygons: TQRPolygons): Boolean;
var
    i:                           Byte;
    index:                       NativeUInt;
    polygon:                     TQRPolygon;
    leftResolved, rightResolved: Boolean;
begin
    // no node to resolve? (this should never happen, but...)
    if (not Assigned(pNode)) then
        Exit(False);

    leftResolved  := False;
    rightResolved := False;

    // is leaf?
    if ((not Assigned(pNode.m_pLeft)) and (not Assigned(pNode.m_pRight))) then
    begin
        // iterate through polygons
        for polygon in pNode.m_Polygons do
        begin
            // get current output list index
            index := Length(polygons);

            // add new polygon to output list
            SetLength(polygons, index + 1);

            // copy polygon content
            for i := 0 to 2 do
                polygons[index].SetVertex(i, polygon.GetVertex(i));
        end;

        Exit(True);
    end;

    // check if ray intersects the left box
    if (Assigned(pNode.m_pLeft) and TQRCollisionHelper.GetRayBoxCollision(pRay, pNode.m_pLeft.m_pBox)) then
        // resolve left node
        leftResolved := Resolve(pRay, pNode.m_pLeft, polygons);

    // check if ray intersects the right box
    if (Assigned(pNode.m_pRight) and TQRCollisionHelper.GetRayBoxCollision(pRay, pNode.m_pRight.m_pBox)) then
        // resolve right node
        rightResolved := Resolve(pRay, pNode.m_pRight, polygons);

    Result := (leftResolved or rightResolved);
end;
//--------------------------------------------------------------------------------------------------
function TQRAABBTree.Populate(const polygons: TQRPolygons;
                                 hIsCanceled: TQRIsCanceledEvent): Boolean;
begin
    // tree was already populated?
    if (Assigned(m_pRoot)) then
        // clear it first
        Release(m_pRoot);

    // create root node
    New(m_pRoot);
    m_pRoot.m_pParent := nil;

    // populate tree
    Result := Populate(m_pRoot, polygons, hIsCanceled);
end;
//--------------------------------------------------------------------------------------------------
function TQRAABBTree.Resolve(const pRay: TQRRay; var polygons: TQRPolygons): Boolean;
begin
    Result := Resolve(pRay, m_pRoot, polygons);
end;
//--------------------------------------------------------------------------------------------------
// TQRCollisionHelper
//--------------------------------------------------------------------------------------------------
class procedure TQRCollisionHelper.AddPolygon(const vb: TQRVertexBuffer;
                                            v1, v2, v3: NativeUInt;
                                          var polygons: TQRPolygons);
var
    polygonIndex: NativeUInt;
begin
    // get current polygon index
    polygonIndex := Length(polygons);

    // add new polygon to output list
    SetLength(polygons, polygonIndex + 1);

    // copy polygon from source buffer
    polygons[polygonIndex].SetVertex(0, TQRVector3D.Create(vb[v1], vb[v1 + 1], vb[v1 + 2]));
    polygons[polygonIndex].SetVertex(1, TQRVector3D.Create(vb[v2], vb[v2 + 1], vb[v2 + 2]));
    polygons[polygonIndex].SetVertex(2, TQRVector3D.Create(vb[v3], vb[v3 + 1], vb[v3 + 2]));
end;
//--------------------------------------------------------------------------------------------------
class function TQRCollisionHelper.GetRayPolygonCollision(const pRay: TQRRay;
                                                      const polygon: TQRPolygon): Boolean;
var
    polygonPlane: TQRPlane;
    pointOnPlane: TQRVector3D;
begin
    // create a plane using the 3 vertices of the polygon
    polygonPlane := polygon.GetPlane;

    // calculate the intersection point
    if (not polygonPlane.IntersectRay(pRay.Pos^, pRay.Dir^, pointOnPlane)) then
        Exit(False);

    // check if calculated point is inside the polygon
    Result := polygon.Inside(pointOnPlane);
end;
//--------------------------------------------------------------------------------------------------
class function TQRCollisionHelper.GetRayBoxCollision(const pRay: TQRRay; const pBox: PQRBox): Boolean;
var
    tx1, tx2, ty1, ty2, tz1, tz2, txn, txf, tyn, tyf, tzn, tzf, tnear, tfar: Single;
begin
    // no ray to check?
    if (not Assigned(pRay)) then
        Exit(False);

    // no box to check against?
    if (not Assigned(pBox)) then
        Exit(False);

    // calculate nearest point where ray intersects box on x coordinate
    if (not IsInfinite(pRay.InvDir.X)) then
        tx1 := ((pBox.Min.X - pRay.Pos.X) * pRay.InvDir.X)
    else
    if ((pBox.Min.X - pRay.Pos.X) < 0.0) then
        tx1 := NegInfinity
    else
        tx1 := Infinity;

    // calculate farthest point where ray intersects box on x coordinate
    if (not IsInfinite(pRay.InvDir.X)) then
        tx2 := ((pBox.Max.X - pRay.Pos.X) * pRay.InvDir.X)
    else
    if ((pBox.Max.X - pRay.Pos.X) < 0.0) then
        tx2 := NegInfinity
    else
        tx2 := Infinity;

    // calculate nearest point where ray intersects box on y coordinate
    if (not IsInfinite(pRay.InvDir.Y)) then
        ty1 := ((pBox.Min.Y - pRay.Pos.Y) * pRay.InvDir.Y)
    else
    if ((pBox.Min.Y - pRay.Pos.Y) < 0.0) then
        ty1 := NegInfinity
    else
        ty1 := Infinity;

    // calculate farthest point where ray intersects box on y coordinate
    if (not IsInfinite(pRay.InvDir.Y)) then
        ty2 := ((pBox.Max.Y - pRay.Pos.Y) * pRay.InvDir.Y)
    else
    if ((pBox.Max.Y - pRay.Pos.Y) < 0.0) then
        ty2 := NegInfinity
    else
        ty2 := Infinity;

    // calculate nearest point where ray intersects box on z coordinate
    if (not IsInfinite(pRay.InvDir.Z)) then
        tz1 := ((pBox.Min.Z - pRay.Pos.Z) * pRay.InvDir.Z)
    else
    if ((pBox.Min.Z - pRay.Pos.Z) < 0.0) then
        tz1 := NegInfinity
    else
        tz1 := Infinity;

    // calculate farthest point where ray intersects box on z coordinate
    if (not IsInfinite(pRay.InvDir.Z)) then
        tz2 := ((pBox.Max.Z - pRay.Pos.Z) * pRay.InvDir.Z)
    else
    if ((pBox.Max.Z - pRay.Pos.Z) < 0.0) then
        tz2 := NegInfinity
    else
        tz2 := Infinity;

    // calculate near/far intersection on each axis
    txn := Min(tx1, tx2);
    txf := Max(tx1, tx2);
    tyn := Min(ty1, ty2);
    tyf := Max(ty1, ty2);
    tzn := Min(tz1, tz2);
    tzf := Max(tz1, tz2);

    // calculate final near/far intersection point
    tnear := Max(tyn, tzn);
    tnear := Max(txn, tnear);
    tfar  := Min(tyf, tzf);
    tfar  := Min(txf, tfar);

    // check if ray intersects box
    Result := (tfar >= tnear);
end;
//--------------------------------------------------------------------------------------------------
class function TQRCollisionHelper.GetPolygons(const vertex: TQRVertex;
                                              var polygons: TQRPolygons;
                                               hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    i, index, vbLength, stripLength, fanLength, step, v1, v2, v3, v4: NativeUInt;
begin
    vbLength := Length(vertex.m_Buffer);

    // no data to extract from?
    if (vbLength = 0) then
        Exit(True);

    // search for vertex type
    case vertex.m_Type of
        EQR_VT_Triangles:
        begin
            // calculate iteration step
            step := (vertex.m_Stride * 3);
            i    := 0;

            // iterate through source vertices
            while (i < vbLength) do
            begin
                // is canceled?
                if (Assigned(hIsCanceled) and hIsCanceled) then
                    Exit(False);

                AddPolygon(vertex.m_Buffer,
                           i,
                           i +  vertex.m_Stride,
                           i + (vertex.m_Stride * 2),
                           polygons);

                Inc(i, step);
            end;

            Exit(True);
        end;

        EQR_VT_TriangleStrip:
        begin
            // calculate length to read in triangle strip buffer
            stripLength := (vbLength - (vertex.m_Stride * 2));
            i           := 0;
            index       := 0;

            // iterate through source vertices
            while (i < stripLength) do
            begin
                // is canceled?
                if (Assigned(hIsCanceled) and hIsCanceled) then
                    Exit(False);

                // extract polygon from source buffer, revert odd polygons
                if ((index = 0) or ((index mod 2) = 0)) then
                    AddPolygon(vertex.m_Buffer,
                               i,
                               i +  vertex.m_Stride,
                               i + (vertex.m_Stride * 2),
                               polygons)
                else
                    AddPolygon(vertex.m_Buffer,
                               i +  vertex.m_Stride,
                               i,
                               i + (vertex.m_Stride * 2),
                               polygons);

                Inc(i, vertex.m_Stride);
                Inc(index);
            end;

            Exit(True);
        end;

        EQR_VT_TriangleFan:
        begin
            // calculate length to read in triangle fan buffer
            fanLength := (vbLength - vertex.m_Stride);
            i         := vertex.m_Stride;

            // iterate through source vertices
            while (i < fanLength) do
            begin
                // is canceled?
                if (Assigned(hIsCanceled) and hIsCanceled) then
                    Exit(False);

                // extract polygon from source buffer
                AddPolygon(vertex.m_Buffer,
                           0,
                           i,
                           i + vertex.m_Stride,
                           polygons);

                Inc(i, vertex.m_Stride);
            end;

            Exit(True);
        end;

        EQR_VT_Quads:
        begin
            // calculate iteration step
            step := (vertex.m_Stride * 4);
            i    := 0;

            // iterate through source vertices
            while (i < vbLength) do
            begin
                // is canceled?
                if (Assigned(hIsCanceled) and hIsCanceled) then
                    Exit(False);

                // calculate vertices position
                v1 := i;
                v2 := i +  vertex.m_Stride;
                v3 := i + (vertex.m_Stride * 2);
                v4 := i + (vertex.m_Stride * 3);

                // extract polygons from source buffer
                AddPolygon(vertex.m_Buffer, v1, v2, v3, polygons);
                AddPolygon(vertex.m_Buffer, v3, v2, v4, polygons);

                Inc(i, step);
            end;

            Exit(True);
        end;

        EQR_VT_QuadStrip:
        begin
            // calculate iteration step
            step := (vertex.m_Stride * 2);

            // calculate length to read in triangle strip buffer
            stripLength := (vbLength - (vertex.m_Stride * 2));
            i           := 0;

            // iterate through source vertices
            while (i < stripLength) do
            begin
                // is canceled?
                if (Assigned(hIsCanceled) and hIsCanceled) then
                    Exit(False);

                // calculate vertices position
                v1 := i;
                v2 := i +  vertex.m_Stride;
                v3 := i + (vertex.m_Stride * 2);
                v4 := i + (vertex.m_Stride * 3);

                // extract polygons from source buffer
                AddPolygon(vertex.m_Buffer, v1, v2, v3, polygons);
                AddPolygon(vertex.m_Buffer, v3, v2, v4, polygons);

                Inc(i, step);
            end;

            Exit(True);
        end;
    else
        Result := False;
    end;
end;
//--------------------------------------------------------------------------------------------------

end.
