// *************************************************************************************************
// * ==> UTQRModel --------------------------------------------------------------------------------*
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
 @abstract(@name provides the features to load a model and build his vertex buffer.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRModel;

{$MODE Delphi}

interface

uses Classes,
     SysUtils,
     UTQRCommon,
     UTQRCache,
     UTQRGraphics,
     UTQRGeometry,
     UTQR3D,
     UTQRCollision;

type
    {$REGION 'Documentation'}
    {**
     Model helper
    }
    {$ENDREGION}
    TQRModelHelper = class
        public
            {$REGION 'Documentation'}
            {**
             Checks if there is enough remaining space in buffer to read next data
             @param(pBuffer Buffer in which data should be read)
             @param(lengthToRead Data length to read in buffer)
             @param(errorMsg @bold([out]) On error, contains an error message to show)
             @return(@true if next data can be read in buffer, otherwise @false)
            }
            {$ENDREGION}
            class function ValidateNextRead(pBuffer: TStream;
                                       lengthToRead: NativeUInt;
                                       out errorMsg: UnicodeString): Boolean; static;

            {$REGION 'Documentation'}
            {**
             Populate aligned-axis bounding box tree
             @param(mesh Source mesh from which aligned-axis bounding box tree should be populated)
             @param(pAABBTree Aligned-axis bounding box tree to populate, ignored if @nil)
             @param(hIsCanceled Is canceled callback function to use, ignored if @nil)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            class function PopulateAABBTree(const mesh: TQRMesh;
                                             pAABBTree: TQRAABBTree;
                                           hIsCanceled: TQRIsCanceledEvent = nil): Boolean; static;

            {$REGION 'Documentation'}
            {**
             Interpolates mesh
             @param(position Interpolation position, in percent (between 0.0 and 1.0))
             @param(mesh1 First mesh to interpolate)
             @param(mesh2 Second mesh to interpolate)
             @param(mesh @bold([out]) Resulting interpolated mesh)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) This function should only be used for compatibility with old OpenGL 1.x
                             versions, as normally interpolation should be done in vertex shader
            }
            {$ENDREGION}
            class function Interpolate(const position: Single;
                                   const mesh1, mesh2: TQRMesh;
                                             out mesh: TQRMesh): Boolean; static;
    end;

    {$REGION 'Documentation'}
    {**
     Model cache item, cache all data needed to render a model, detect model collisions, ...
    }
    {$ENDREGION}
    TQRModelCache = class
        private
            m_pMeshCache:     TQRCache<NativeUInt, PQRMesh>;
            m_pAABBTreeCache: TQRCache<NativeUInt, TQRAABBTree>;

        protected
            {$REGION 'Documentation'}
            {**
             Called when mesh is deleted from cache
             @param(key @bold([in, out]) Deleting key)
             @param(pMesh @bold([in, out]) Deleting value)
             @return(@true if value can be deleted from cache, otherwise @false)
            }
            {$ENDREGION}
            function OnDeleteMesh(const key: NativeUInt; var pMesh: PQRMesh): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Called when aligned-axis bounding box tree is deleted from cache
             @param(key @bold([in, out]) Deleting key)
             @param(pTree @bold([in, out]) Deleting value)
             @return(@true if value can be deleted from cache, otherwise @false)
            }
            {$ENDREGION}
            function OnDeleteAABBTree(const key: NativeUInt; var pTree: TQRAABBTree): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Gets mesh at index
             @param(index Index)
             @return(Mesh)
            }
            {$ENDREGION}
            function GetMesh(index: NativeUInt): PQRMesh; virtual;

            {$REGION 'Documentation'}
            {**
             Sets mesh at index
             @param(index Index)
             @param(pMesh Mesh to set)
             @br @bold(NOTE) Be careful, the internal cache will take the mesh ownership, so don't
                             try to delete it externally
             }
            {$ENDREGION}
            procedure SetMesh(index: NativeUInt; pMesh: PQRMesh); virtual;

            {$REGION 'Documentation'}
            {**
             Gets aligned-axis bounding box tree at index
             @param(index Index)
             @return(Tree)
            }
            {$ENDREGION}
            function GetTree(index: NativeUInt): TQRAABBTree; virtual;

            {$REGION 'Documentation'}
            {**
             Sets aligned-axis bounding box tree at index
             @param(index Index)
             @param(pTree Tree to set)
             @br @bold(NOTE) Be careful, the internal cache will take the tree ownership, so don't
                             try to delete it externally
            }
            {$ENDREGION}
            procedure SetTree(index: NativeUInt; pTree: TQRAABBTree); virtual;

            {$REGION 'Documentation'}
            {**
             Gets the mesh count
             @return(The mesh count)
            }
            {$ENDREGION}
            function GetMeshCount: NativeUInt; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the aligned-axis bounding box tree count
             @return(The aligned-axis bounding box tree count)
            }
            {$ENDREGION}
            function GetAABBTreeCount: NativeUInt; virtual;

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

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the mesh at index
            }
            {$ENDREGION}
            property Mesh[index: NativeUInt]: PQRMesh read GetMesh write SetMesh;

            {$REGION 'Documentation'}
            {**
             Gets or sets the aligned-axis bounding box at index
            }
            {$ENDREGION}
            property AABBTree[index: NativeUInt]: TQRAABBTree read GetTree write SetTree;

            {$REGION 'Documentation'}
            {**
             Gets the mesh count
            }
            {$ENDREGION}
            property MeshCount: NativeUInt read GetMeshCount;

            {$REGION 'Documentation'}
            {**
             Gets the aligned-axis bounding box tree count
            }
            {$ENDREGION}
            property AABBTreeCount: NativeUInt read GetAABBTreeCount;
    end;

    {$REGION 'Documentation'}
    {**
     Basic model parser
    }
    {$ENDREGION}
    TQRModelParser = class
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
             Loads model from file
             @param(fileName File name)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Load(const fileName: TFileName): Boolean; overload; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Loads model from buffer
             @param(pBuffer Buffer)
             @param(length Length to read in buffer, in bytes)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Read will begin from current offset
            }
            {$ENDREGION}
            function Load(const pBuffer: TStream;
                                 length: NativeUInt): Boolean; overload; virtual; abstract;
    end;

    {$REGION 'Documentation'}
    {**
     Basic 3D model
    }
    {$ENDREGION}
    TQRModel = class
        private
            m_pColor:       TQRColor;
            m_VertexFormat: TQRVertexFormat;

        protected
            {$REGION 'Documentation'}
            {**
             Gets model color
             @return(Model color)
            }
            {$ENDREGION}
            function GetColor: TQRColor; virtual;

            {$REGION 'Documentation'}
            {**
             Sets model color
             @param(pColor Model color to set)
            }
            {$ENDREGION}
            procedure SetColor(const pColor: TQRColor); virtual;

            {$REGION 'Documentation'}
            {**
             Gets vertex format
             @return(Vertex format)
            }
            {$ENDREGION}
            function GetVertexFormat: TQRVertexFormat; virtual;

            {$REGION 'Documentation'}
            {**
             Gets vertex format
             @param(value Vertex format to set)
            }
            {$ENDREGION}
            procedure SetVertexFormat(value: TQRVertexFormat); virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; virtual;

            {$REGION 'Documentation'}
            {**
            * Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the model color
            }
            {$ENDREGION}
            property Color: TQRColor read GetColor write SetColor;

            {$REGION 'Documentation'}
            {**
             Gets or sets the vertex format to use
            }
            {$ENDREGION}
            property VertexFormat: TQRVertexFormat read GetVertexFormat write SetVertexFormat;
    end;

    {$REGION 'Documentation'}
    {**
     Basic 3D static model. A static model is a simple model without animation, as e.g. a sphere,
     or a frontwave model
    }
    {$ENDREGION}
    TQRStaticModel = class(TQRModel)
        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Gets model mesh
             @param(mesh @bold([out]) Mesh)
             @param(pAABBTree Aligned-axis bounding box tree to populate, ignored if @nil)
             @param(hIsCanceled Callback function that allows to break the operation, can be @nil)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function GetMesh(out mesh: TQRMesh;
                            pAABBTree: TQRAABBTree;
                          hIsCanceled: TQRIsCanceledEvent): Boolean; virtual; abstract;
    end;

    {$REGION 'Documentation'}
    {**
     Basic 3D framed model. A framed model is a model in which the animation is done frame by frame,
     as e.g. a md2 model
    }
    {$ENDREGION}
    TQRFramedModel = class(TQRModel)
        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Gets model frame mesh
             @param(index Frame mesh index to create)
             @param(mesh @bold([out]) Frame mesh)
             @param(pAABBTree Aligned-axis bounding box tree to populate, ignored if @nil)
             @param(hIsCanceled Callback function that allows to break the operation, can be @nil)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function GetMesh(index: NativeUInt;
                          out mesh: TQRMesh;
                         pAABBTree: TQRAABBTree;
                       hIsCanceled: TQRIsCanceledEvent): Boolean; overload; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Gets model frame mesh
             @param(index Frame mesh index to get)
             @param(nextIndex Frame mesh index to interpolate with)
             @param(interpolationFactor Interpolation factor to apply)
             @param(mesh @bold([out]) Frame mesh)
             @param(hIsCanceled Callback function that allows to break the operation, can be @nil)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function GetMesh(index, nextIndex: NativeUInt;
                          interpolationFactor: Double;
                                     out mesh: TQRMesh;
                                  hIsCanceled: TQRIsCanceledEvent): Boolean; overload; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Gets mesh count
             @return(Mesh count)
            }
            {$ENDREGION}
            function GetMeshCount: NativeUInt; virtual; abstract;
    end;

    {$REGION 'Documentation'}
    {**
     Basic 3D articulated model. An articulated model is a model in which animation is based on
     bones, or any kind of articulable structure, as e.g. 3ds models, x models or md5 models
    }
    {$ENDREGION}
    TQRArticulatedModel = class(TQRModel)
        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRModelHelper
//--------------------------------------------------------------------------------------------------
class function TQRModelHelper.ValidateNextRead(pBuffer: TStream;
                                          lengthToRead: NativeUInt;
                                          out errorMsg: UnicodeString): Boolean;
begin
    // check if buffer is large enough to read next data block
    if ((pBuffer.Position + lengthToRead) <= pBuffer.Size) then
        Exit(True);

    // build error message containing debug informations
    errorMsg := UnicodeString('offset - '              + IntToStr(pBuffer.Position)                +
                              ' - remaining length - ' + IntToStr(pBuffer.Size - pBuffer.Position) +
                              ' - length to read - '   + IntToStr(lengthToRead));

    Result := False;
end;
//--------------------------------------------------------------------------------------------------
class function TQRModelHelper.PopulateAABBTree(const mesh: TQRMesh;
                                                pAABBTree: TQRAABBTree;
                                              hIsCanceled: TQRIsCanceledEvent): Boolean;
var
    vertex:   TQRVertex;
    polygons: TQRPolygons;
begin
    // no destination tree?
    if (not Assigned(pAABBTree)) then
        // it's not an error so return true
        Exit(True);

    // iterate through meshes
    for vertex in mesh do
        // get collide polygons
        if (not TQRCollisionHelper.GetPolygons(vertex, polygons, hIsCanceled)) then
            Exit(False);

    // populate aligned-axis bounding box tree
    Result := pAABBTree.Populate(polygons, hIsCanceled);
end;
//--------------------------------------------------------------------------------------------------
class function TQRModelHelper.Interpolate(const position: Single;
                                      const mesh1, mesh2: TQRMesh;
                                                out mesh: TQRMesh): Boolean;
var
    count, bufferCount, i, j, index:                   NativeInt;
    vertex:                                            TQRVertex;
    srcVec, dstVec, vec, srcNormal, dstNormal, normal: TQRVector3D;
begin
    // get vertice count
    count := Length(mesh1);

    // are mesh compatible?
    if (count <> Length(mesh2)) then
        Exit(False);

    // is mesh empty?
    if (count = 0) then
        Exit(False);

    // iterate through mesh to interpolate
    for i := 0 to count - 1 do
    begin
        // are frame compatibles?
        if (not mesh1[i].CompareFormat(mesh2[i])) then
            Exit(False);

        // not a 3D coordinate?
        if (mesh1[i].m_CoordType <> EQR_VC_XYZ) then
            Exit(False);

        vertex.m_Name      := mesh1[i].m_Name;
        vertex.m_Stride    := mesh1[i].m_Stride;
        vertex.m_Type      := mesh1[i].m_Type;
        vertex.m_Format    := mesh1[i].m_Format;
        vertex.m_CoordType := mesh1[i].m_CoordType;

        // get vertex buffer data count
        bufferCount := Length(mesh1[i].m_Buffer);

        j := 0;

        // iterate through vertex buffer content
        while (j < bufferCount) do
        begin
            index := 3;

            // get positions
            srcVec.X := mesh1[i].m_Buffer[j];
            srcVec.Y := mesh1[i].m_Buffer[j + 1];
            srcVec.Z := mesh1[i].m_Buffer[j + 2];
            dstVec.X := mesh2[i].m_Buffer[j];
            dstVec.Y := mesh2[i].m_Buffer[j + 1];
            dstVec.Z := mesh2[i].m_Buffer[j + 2];

            // interpolate positions
            vec := srcVec.Interpolate(dstVec, position);

            // set interpolated positions in destination buffer
            SetLength(vertex.m_Buffer, Length(vertex.m_Buffer) + 3);
            vertex.m_Buffer[j]     := vec.X;
            vertex.m_Buffer[j + 1] := vec.Y;
            vertex.m_Buffer[j + 2] := vec.Z;

            // do include normals?
            if (EQR_VF_Normals in mesh1[i].m_Format) then
            begin
                // get normals
                srcNormal.X := mesh1[i].m_Buffer[j + index];
                srcNormal.Y := mesh1[i].m_Buffer[j + index + 1];
                srcNormal.Z := mesh1[i].m_Buffer[j + index + 2];
                dstNormal.X := mesh2[i].m_Buffer[j + index];
                dstNormal.Y := mesh2[i].m_Buffer[j + index + 1];
                dstNormal.Z := mesh2[i].m_Buffer[j + index + 2];

                // interpolate normals
                normal := srcNormal.Interpolate(dstNormal, position);

                // set interpolated normals in destination buffer
                SetLength(vertex.m_Buffer, Length(vertex.m_Buffer) + 3);
                vertex.m_Buffer[j + index]     := normal.X;
                vertex.m_Buffer[j + index + 1] := normal.Y;
                vertex.m_Buffer[j + index + 2] := normal.Z;
                Inc(index, 3);
            end;

            // do include texture coordinates?
            if (EQR_VF_TexCoords in mesh1[i].m_Format) then
            begin
                // copy texture coordinates from source
                SetLength(vertex.m_Buffer, Length(vertex.m_Buffer) + 2);
                vertex.m_Buffer[j + index]     := mesh1[i].m_Buffer[j + index];
                vertex.m_Buffer[j + index + 1] := mesh1[i].m_Buffer[j + index + 1];
                Inc(index, 2);
            end;

            // do include colors?
            if (EQR_VF_Colors in mesh1[i].m_Format) then
            begin
                // copy color from source
                SetLength(vertex.m_Buffer, Length(vertex.m_Buffer) + 4);
                vertex.m_Buffer[j + index]     := mesh1[i].m_Buffer[j + index];
                vertex.m_Buffer[j + index + 1] := mesh1[i].m_Buffer[j + index + 1];
                vertex.m_Buffer[j + index + 2] := mesh1[i].m_Buffer[j + index + 2];
                vertex.m_Buffer[j + index + 3] := mesh1[i].m_Buffer[j + index + 3];
            end;

            Inc(j, mesh1[i].m_Stride);
        end;

        // add interpolated mesh to output list
        SetLength(mesh, Length(mesh) + 1);
        mesh[Length(mesh) - 1] := vertex;

        // delete vertex
        SetLength(vertex.m_Buffer, 0);
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TQRModelCache
//--------------------------------------------------------------------------------------------------
constructor TQRModelCache.Create;
begin
    inherited Create;

    m_pMeshCache     := TQRCache<NativeUInt, PQRMesh>.Create;
    m_pAABBTreeCache := TQRCache<NativeUInt, TQRAABBTree>.Create;

    // set callbacks
    m_pMeshCache.OnDeleteFromCache     := OnDeleteMesh;
    m_pAABBTreeCache.OnDeleteFromCache := OnDeleteAABBTree;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRModelCache.Destroy;
begin
    // clear memory
    m_pAABBTreeCache.Free;
    m_pMeshCache.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelCache.OnDeleteMesh(const key: NativeUInt; var pMesh: PQRMesh): Boolean;
begin
    if (Assigned(pMesh)) then
        Dispose(pMesh);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelCache.OnDeleteAABBTree(const key: NativeUInt; var pTree: TQRAABBTree): Boolean;
begin
    pTree.Free;
    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelCache.GetMesh(index: NativeUInt): PQRMesh;
begin
    // get mesh from cache if exists, otherwise returns nil
    if (not m_pMeshCache.Get(index, Result)) then
        Result := nil;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelCache.SetMesh(index: NativeUInt; pMesh: PQRMesh);
begin
    // cache mesh, clear the previous entry if exists. Be careful, the cache will take the ownership
    // of the received mesh, so don't try to delete it externally
    m_pMeshCache.Add(index, pMesh);
end;
//--------------------------------------------------------------------------------------------------
function TQRModelCache.GetTree(index: NativeUInt): TQRAABBTree;
begin
    // get tree from cache if exists, otherwise returns nil
    if (not m_pAABBTreeCache.Get(index, Result)) then
        Result := nil;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModelCache.SetTree(index: NativeUInt; pTree: TQRAABBTree);
begin
    // cache tree, clear the previous entry if exists. Be careful, the cache will take the ownership
    // of the received tree, so don't try to delete it externally
    m_pAABBTreeCache.Add(index, pTree);
end;
//--------------------------------------------------------------------------------------------------
function TQRModelCache.GetMeshCount: NativeUInt;
begin
    Result := m_pMeshCache.Count;
end;
//--------------------------------------------------------------------------------------------------
function TQRModelCache.GetAABBTreeCount: NativeUInt;
begin
    Result := m_pAABBTreeCache.Count;
end;
//--------------------------------------------------------------------------------------------------
// TQRModelParser
//--------------------------------------------------------------------------------------------------
constructor TQRModelParser.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRModelParser.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
// TQRModel
//--------------------------------------------------------------------------------------------------
constructor TQRModel.Create;
begin
    inherited Create;

    m_pColor := TQRColor.Create(255, 255, 255, 255);
end;
//--------------------------------------------------------------------------------------------------
destructor TQRModel.Destroy;
begin
    m_pColor.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRModel.GetColor: TQRColor;
begin
    Result := m_pColor;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModel.SetColor(const pColor: TQRColor);
begin
    m_pColor.Assign(pColor);
end;
//--------------------------------------------------------------------------------------------------
function TQRModel.GetVertexFormat: TQRVertexFormat;
begin
    Result := m_VertexFormat;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRModel.SetVertexFormat(value: TQRVertexFormat);
begin
    m_VertexFormat := value;
end;
//--------------------------------------------------------------------------------------------------
// TQRStaticModel
//--------------------------------------------------------------------------------------------------
constructor TQRStaticModel.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRStaticModel.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
// TQRFramedModel
//--------------------------------------------------------------------------------------------------
constructor TQRFramedModel.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRFramedModel.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
// TQRArticulatedModel
//--------------------------------------------------------------------------------------------------
constructor TQRArticulatedModel.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRArticulatedModel.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------

end.
