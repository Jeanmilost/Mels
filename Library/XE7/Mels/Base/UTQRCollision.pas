{**************************************************************************************************
 * ==> UTQRCollisions ----------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides classes and fuctions for collision detection                *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRCollision;

interface

uses System.Math,
     UTQRCommon,
     UTQRGeometry,
     UTQR3D;

const
    QR_Epsilon = 1.0E-3; // epsilon value used for tolerance

type
    {**
    * Aligned-axis bounding box tree node pointer (needed to be declared before record itself)
    *}
    PQRAABBNode = ^TQRAABBNode;

    {**
    * Aligned-axis bounding box tree node
    *}
    TQRAABBNode = record
        m_pParent:  PQRAABBNode;
        m_pLeft:    PQRAABBNode;
        m_pRight:   PQRAABBNode;
        m_pBox:     PQRBox;
        m_Polygons: TQRPolygons;
    end;

    {**
    * Aligned-axis bounding box tree
    *}
    TQRAABBTree = class
        protected
            m_pRoot: PQRAABBNode;

            {**
            * Releases tree content
            *@param pNode - root node from which content should be released
            *}
            procedure Release(pNode: PQRAABBNode); virtual;

            {**
            * Checks if a value is between 2 values
            *@param value - value to test
            *@param valueStart - start value
            *@param valueEnd - end value
            *@param epsilon - epsilon value for tolerance
            *@return true if value is between values, otherwise false
            *}
            function ValueIsBetween(const value, valueStart, valueEnd, epsilon: Single): Boolean; virtual;

            {**
            * Checks if a point is between 2 vertices
            *@param point - point to test
            *@param pointStart - start vertex
            *@param pointEnd - End vertex
            *@param epsilon - epsilon value for tolerance
            *@return true if value is between points, otherwise false
            *}
            function VectorIsBetween(const point, pointStart, pointEnd: TQRVector3D;
                                                         const epsilon: Single): Boolean; virtual;

            {**
            * Adds a polygon inside an existing bounding box
            *@param polygon - polygon to add
            *@param box - bounding box in which polygon should be added
            *@param empty[in, out] - if true, box is empty an still no contains any polygon
            *}
            procedure AddPolygonToBoundingBox(polygon: TQRPolygon;
                                                 pBox: PQRBox;
                                            var empty: Boolean); virtual;

            {**
            * Populates AABB tree
            *@param pNode - root or parent node to create from
            *@param pPolygons - source polygon array
            *@param hIsCanceled - callback function that allows to break the operation, can be nil
            *@return true on success, otherwise false
            *}
            function Populate(pNode: PQRAABBNode;
                     const polygons: TQRPolygons;
                        hIsCanceled: TQRIsCanceledEvent): Boolean; overload; virtual;

            {**
            * Resolves AABB tree
            *@param pRay - ray against which tree boxes will be tested
            *@param pNode - root or parent node to resolve
            *@param[in, out] pPolygons - polygons belonging to boxes hit by ray
            *@return true on success, otherwise false
            *@note Polygon list content should be deleted when useless
            *}
            function Resolve(const pRay: TQRRay;
                            const pNode: PQRAABBNode;
                           var polygons: TQRPolygons): Boolean; overload; virtual;

        public
            { Construction/Destruction }
            constructor Create();  virtual;
            destructor  Destroy(); override;

            {**
            * Populates AABB tree
            *@param pPolygons - source polygon array
            *@param hIsCanceled - callback function that allows to break the operation, can be nil
            *@return true on success, otherwise false
            *}
            function Populate(const polygons: TQRPolygons;
                                 hIsCanceled: TQRIsCanceledEvent = nil): Boolean; overload; virtual;

            {**
            * Resolves AABB tree
            *@param pRay - ray against which tree boxes will be tested
            *@param[in, out] pPolygons - polygons belonging to boxes hit by ray
            *@return true on success, otherwise false
            *@note Polygon list content should be deleted when useless
            *}
            function Resolve(const pRay: TQRRay;
                           var polygons: TQRPolygons): Boolean; overload; virtual;
    end;

    {**
    * 3D collision detection helper
    *}
    TQRCollisionHelper = class
        protected
            {**
            * Adds polygon to array
            *@param vb - source vertex buffer
            *@param v1 - first polygon vertex index in vertex buffer
            *@param v2 - second polygon vertex index in vertex buffer
            *@param v3 - third polygon vertex index in vertex buffer
            *@param[in, out] polygons - polygon array that contains generated polygons
            *@note Generated polygons should be deleted when useless
            *}
            class procedure AddPolygon(const vb: TQRVertexBuffer;
                                     v1, v2, v3: NativeUInt;
                                   var polygons: TQRPolygons); static;

        public
            {**
            * Tests collision between a ray and a polygon
            *@param pRay - ray
            *@param polygon - polygon to check
            *@return true if ray intersects polygon, otherwise false
            *}
            class function GetRayPolygonCollision(const pRay: TQRRay;
                                               const polygon: TQRPolygon): Boolean; static;

            {**
            * Tests collision between a ray and a box
            *@param pRay - ray
            *@param pBox - box
            *@return true if ray intersects box, otherwise false
            *}
            class function GetRayBoxCollision(const pRay: TQRRay; const pBox: PQRBox): Boolean; static;

            {**
            * Gets polygons from vertex
            *@param vertex - source vertex descriptor, contains buffer to get from
            *@param[in, out] polygons - polygon list to populate
            *@param hIsCanceled - callback function that allows to break the operation, can be nil
            *@return true on success, otherwise false
            *@note Polygon list content should be deleted when useless
            *}
            class function GetPolygons(const vertex: TQRVertex;
                                       var polygons: TQRPolygons;
                                        hIsCanceled: TQRIsCanceledEvent = nil): Boolean; static;
    end;

implementation
//------------------------------------------------------------------------------
// TQRAABBTree
//------------------------------------------------------------------------------
constructor TQRAABBTree.Create();
begin
    inherited Create;

    m_pRoot := nil;
end;
//------------------------------------------------------------------------------
destructor  TQRAABBTree.Destroy();
begin
    // delete entire tree hierarchy
    Release(m_pRoot);

    inherited Destroy;
end;
//------------------------------------------------------------------------------
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
//------------------------------------------------------------------------------
function TQRAABBTree.ValueIsBetween(const value, valueStart, valueEnd, epsilon: Single): Boolean;
var
    minVal, maxVal: Single;
begin
    minVal := Min(valueStart, valueEnd);
    maxVal := Max(valueStart, valueEnd);

    // check if each value is between start and end limits considering tolerance
    Result := ((value >= (minVal - epsilon)) and (value <= (maxVal + epsilon)));
end;
//------------------------------------------------------------------------------
function TQRAABBTree.VectorIsBetween(const point, pointStart, pointEnd: TQRVector3D;
                                                         const epsilon: Single): Boolean;
begin
    // check if each vector component is between start and end limits
    Result := (ValueIsBetween(point.X, pointStart.X, pointEnd.X, epsilon) and
               ValueIsBetween(point.Y, pointStart.Y, pointEnd.Y, epsilon) and
               ValueIsBetween(point.Z, pointStart.Z, pointEnd.Z, epsilon));
end;
//------------------------------------------------------------------------------
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
//------------------------------------------------------------------------------
function TQRAABBTree.Populate(pNode: PQRAABBNode;
                     const polygons: TQRPolygons;
                        hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    i,
    polygonIndex,
    leftPolygonIndex,
    rightPolygonIndex,
    polygonCount,
    leftPolygonCount,
    rightPolygonCount:                         NativeUInt;
    j, k:                                      Byte;
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
    if (Assigned(hIsCanceled) and hIsCanceled()) then
        Exit;

    // create a collision box
    New(pNode.m_pBox);

    // get polygon count
    polygonCount := Length(polygons);

    // no polygons? (should never happen, but...)
    if (polygonCount > 0) then
        // iterate through polygons to divide
        for i := 0 to polygonCount - 1 do
        begin
            // is canceled?
            if (Assigned(hIsCanceled) and hIsCanceled()) then
                Exit;

            // calculate bounding box
            AddPolygonToBoundingBox(polygons[i], pNode.m_pBox, boxEmpty);
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
        for i := 0 to polygonCount - 1 do
            for j := 0 to 2 do
            begin
                // is canceled?
                if (Assigned(hIsCanceled) and hIsCanceled()) then
                    Exit;

                // check if first polygon vertice belongs to left or right sub-box
                if (VectorIsBetween(polygons[i].GetVertex(j),
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
                    for k := 0 to 2 do
                        leftPolygons[leftPolygonIndex].SetVertex(k, polygons[i].GetVertex(k));

                    break;
                end
                else
                if (VectorIsBetween(polygons[i].GetVertex(j),
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
                    for k := 0 to 2 do
                        rightPolygons[rightPolygonIndex].SetVertex(k, polygons[i].GetVertex(k));

                    break;
                end;
            end;
    finally
        if (Assigned(pLeftBox)) then
            Dispose(pLeftBox);

        if (Assigned(pRightBox)) then
            Dispose(pRightBox);
    end;

    leftPolygonCount  := Length(leftPolygons);
    rightPolygonCount := Length(rightPolygons);
    canResolveLeft    := ((leftPolygonCount  > 0) and (leftPolygonCount  < polygonCount));
    canResolveRight   := ((rightPolygonCount > 0) and (rightPolygonCount < polygonCount));

    // leaf reached?
    if ((not canResolveLeft) and (not canResolveRight)) then
    begin
        // polygons to copy?
        if (polygonCount > 0) then
            // iterate through polygons to copy
            for i := 0 to polygonCount - 1 do
            begin
                // get current polygon index
                polygonIndex := Length(pNode.m_Polygons);

                // add polygon to leaf
                SetLength(pNode.m_Polygons, polygonIndex + 1);

                // copy polygon content
                for j := 0 to 2 do
                    pNode.m_Polygons[polygonIndex].SetVertex(j, polygons[i].GetVertex(j));
            end;

        // delete left and right lists, as they will no more be used
        SetLength(leftPolygons,  0);
        SetLength(rightPolygons, 0);

        Result := True;
        Exit;
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
//------------------------------------------------------------------------------
function TQRAABBTree.Resolve(const pRay: TQRRay;
                            const pNode: PQRAABBNode;
                           var polygons: TQRPolygons): Boolean;
var
    i, index, polygonLength:     NativeUInt;
    j:                           Byte;
    leftResolved, rightResolved: Boolean;
begin
    // no node to resolve? (this should never happen, but...)
    if (not Assigned(pNode)) then
    begin
        Result := False;
        Exit;
    end;

    leftResolved  := False;
    rightResolved := False;

    // is leaf?
    if ((not Assigned(pNode.m_pLeft)) and (not Assigned(pNode.m_pRight))) then
    begin
        // get polygon count the leaf contains
        polygonLength := Length(pNode.m_Polygons);

        // leaf contains polygons?
        if (polygonLength > 0) then
            // iterate through polygons
            for i := 0 to polygonLength - 1 do
            begin
                // get current output list index
                index := Length(polygons);

                // add new polygon to output list
                SetLength(polygons, index + 1);

                // copy polygon content
                for j := 0 to 2 do
                    polygons[index].SetVertex(j, pNode.m_Polygons[i].GetVertex(j));
            end;

        Result := True;
        Exit;
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
//------------------------------------------------------------------------------
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
//------------------------------------------------------------------------------
function TQRAABBTree.Resolve(const pRay: TQRRay; var polygons: TQRPolygons): Boolean;
begin
    Result := Resolve(pRay, m_pRoot, polygons);
end;
//------------------------------------------------------------------------------
// TQRCollisionHelper
//------------------------------------------------------------------------------
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
//------------------------------------------------------------------------------
class function TQRCollisionHelper.GetRayPolygonCollision(const pRay: TQRRay;
                                                      const polygon: TQRPolygon): Boolean;
var
    polygonPlane: TQRPlane;
    pointOnPlane: TQRVector3D;
begin
    // create a plane using the 3 vertices of the polygon
    polygonPlane := polygon.GetPlane();

    // calculate the intersection point
    if (not polygonPlane.IntersectRay(pRay.Pos^, pRay.Dir^, pointOnPlane)) then
    begin
        Result := False;
        Exit;
    end;

    // check if calculated point is inside the polygon
    Result := polygon.Inside(pointOnPlane);
end;
//------------------------------------------------------------------------------
class function TQRCollisionHelper.GetRayBoxCollision(const pRay: TQRRay; const pBox: PQRBox): Boolean;
var
    tx1, tx2, ty1, ty2, tz1, tz2, txn, txf, tyn, tyf, tzn, tzf, tnear, tfar: Single;
begin
    // no ray to check?
    if (not Assigned(pRay)) then
    begin
        Result := False;
        Exit;
    end;

    // no box to check against?
    if (not Assigned(pBox)) then
    begin
        Result := False;
        Exit;
    end;

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
//------------------------------------------------------------------------------
class function TQRCollisionHelper.GetPolygons(const vertex: TQRVertex;
                                              var polygons: TQRPolygons;
                                               hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    i, index, vbLength, stripLength, fanLength, step, v1, v2, v3, v4: NativeUInt;
begin
    vbLength := Length(vertex.m_Buffer);

    // no data to extract from?
    if (vbLength = 0) then
    begin
        Result := True;
        Exit;
    end;

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
                if (Assigned(hIsCanceled) and hIsCanceled()) then
                begin
                    Result := False;
                    Exit;
                end;

                AddPolygon(vertex.m_Buffer,
                           i,
                           i +  vertex.m_Stride,
                           i + (vertex.m_Stride * 2),
                           polygons);

                Inc(i, step);
            end;

            Result := True;
            Exit;
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
                if (Assigned(hIsCanceled) and hIsCanceled()) then
                begin
                    Result := False;
                    Exit;
                end;

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

            Result := True;
            Exit;
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
                if (Assigned(hIsCanceled) and hIsCanceled()) then
                begin
                    Result := False;
                    Exit;
                end;

                // extract polygon from source buffer
                AddPolygon(vertex.m_Buffer,
                           0,
                           i,
                           i + vertex.m_Stride,
                           polygons);

                Inc(i, vertex.m_Stride);
            end;

            Result := True;
            Exit;
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
                if (Assigned(hIsCanceled) and hIsCanceled()) then
                begin
                    Result := False;
                    Exit;
                end;

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

            Result := True;
            Exit;
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
                if (Assigned(hIsCanceled) and hIsCanceled()) then
                begin
                    Result := False;
                    Exit;
                end;

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

            Result := True;
            Exit;
        end;
    else
        Result := False;
    end;
end;
//------------------------------------------------------------------------------

end.
