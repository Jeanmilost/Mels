{**************************************************************************************************
 * ==> UTQRMD2 -----------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides the tools to load a md2 model and build his vertex buffer   *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRMD2;

interface

uses System.Classes,
     System.SysUtils,
     System.Math,
     UTQRCommon,
     UTQRHelpers,
     UTQRGraphics,
     UTQRGeometry,
     UTQR3D,
     UTQRCollision,
     UTQRModel;

const
    //----------------------------------------------------------------------------------------------
    // Global constants
    //----------------------------------------------------------------------------------------------
    CQR_MD2_Mesh_File_Version:          TQRUInt32  = 8;
    CQR_MD2_ID:                         TQRUInt32  = ($32 shl 24) + ($50 shl 16) + ($44 shl 8) + $49;
    CQR_MD2_Normals_Table_File_Version: Single     = 1.0;
    CQR_MD2_Invalid_Index:              NativeUInt = NativeUInt(-1);
    //----------------------------------------------------------------------------------------------

type
    {**
    * MD2 header
    *@note This class cannot be inherited
    *}
    TQRMD2Header = record
        m_ID:                 TQRUInt32;
        m_Version:            TQRUInt32;
        m_SkinWidth:          TQRUInt32;
        m_SkinHeight:         TQRUInt32;
        m_FrameSize:          TQRUInt32;
        m_SkinCount:          TQRUInt32;
        m_VertexCount:        TQRUInt32;
        m_TextureCoordCount:  TQRUInt32;
        m_PolygonCount:       TQRUInt32;
        m_GlCmdsCount:        TQRUInt32;
        m_FrameCount:         TQRUInt32;
        m_SkinOffset:         TQRUInt32;
        m_TextureCoordOffset: TQRUInt32;
        m_PolygonOffset:      TQRUInt32;
        m_FrameOffset:        TQRUInt32;
        m_GlCmdsOffset:       TQRUInt32;
        m_EndOffset:          TQRUInt32;

        {**
        * Reads data from file
        *@param pBuffer - buffer to read from
        *@throw exception on error
        *}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD2Header = ^TQRMD2Header;

    {**
    * MD2 skin
    *@note This class cannot be inherited
    *}
    TQRMD2Skin = record
        m_Name: String;

        {**
        * Reads data from file
        *@param pBuffer - buffer to read from
        *@throw exception on error
        *}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD2Skin= ^TQRMD2Skin;

    {**
    * MD2 vertex
    *@note This class cannot be inherited
    *}
    TQRMD2Vertex = Record
        m_Vertex:      array[0..2] of TQRUInt8;
        m_NormalIndex: TQRUInt8;

        {**
        * Reads data from file
        *@param pBuffer - buffer to read from
        *@throw exception on error
        *}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD2Vertex = ^TQRMD2Vertex;

    {**
    * MD2 texture coordinate
    *@note This class cannot be inherited
    *}
    TQRMD2TextureCoord = Record
        m_U: TQRInt16;
        m_V: TQRInt16;

        {**
        * Reads data from file
        *@param pBuffer - buffer to read from
        *@throw exception on error
        *}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD2TextureCoord = ^TQRMD2TextureCoord;

    {**
    * MD2 frame
    *@note This class cannot be inherited
    *}
    TQRMD2Frame = Record
        m_Name:      String;
        m_Scale:     array[0..2] of TQRFloat32;
        m_Translate: array[0..2] of TQRFloat32;
        m_Vertex:    array of TQRMD2Vertex;

        {**
        * Reads data from file
        *@param pBuffer - buffer to read from
        *@param header - MD2 file header
        *@throw exception on error
        *}
        procedure Read(pBuffer: TStream; const header: TQRMD2Header);
    end;

    PQRMD2Frame = ^TQRMD2Frame;

    {**
    * MD2 polygon
    *@note This class cannot be inherited
    *}
    TQRMD2Polygon = Record
        m_VertexIndex:       array[0..2] of TQRInt16;
        m_TextureCoordIndex: array[0..2] of TQRInt16;

        {**
        * Reads data from file
        *@param pBuffer - buffer to read from
        *@throw exception on error
        *}
        procedure Read(pBuffer: TStream);
    end;

    PQRMD2Polygon = ^TQRMD2Polygon;

    {**
    * Reads and exposes MD2 file content
    *}
    TQRMD2Parser = class(TQRModelParser)
        protected
            m_Header:    TQRMD2Header;
            m_Skins:     array of TQRMD2Skin;
            m_TexCoords: array of TQRMD2TextureCoord;
            m_Polygons:  array of TQRMD2Polygon;
            m_Frames:    array of TQRMD2Frame;
            m_GlCmds:    array of TQRInt32;

            {**
            * Gets header
            *@return header, nil if not found or on error
            *}
            function GetHeader(): PQRMD2Header; virtual;

            {**
            * Gets skin at index
            *@param index - index
            *@return skin, nil if not found or on error
            *}
            function GetSkin(index: NativeInt): PQRMD2Skin; virtual;

            {**
            * Gets texture coordinate at index
            *@param index - index
            *@return texture coordinate, nil if not found or on error
            *}
            function GetTexCoord(index: NativeInt): PQRMD2TextureCoord; virtual;

            {**
            * Gets polygon at index
            *@param index - index
            *@return polygon, nil if not found or on error
            *}
            function GetPolygon(index: NativeInt): PQRMD2Polygon; virtual;

            {**
            * Gets frame at index
            *@param index - index
            *@return frame, nil if not found or on error
            *}
            function GetFrame(index: NativeInt): PQRMD2Frame; virtual;

            {**
            * Gets OpenGL command at index
            *@param index - index
            *@return OpenGL command, -1 if not found or on error
            *}
            function GetGlCmd(index: NativeInt): TQRInt32; virtual;

            {**
            * Gets skin count
            *@return skin count
            *}
            function GetSkinCount(): NativeInt; virtual;

            {**
            * Gets texture coordinate count
            *@return texture coordinate count
            *}
            function GetTexCoordCount(): NativeInt; virtual;

            {**
            * Gets polygon count
            *@return polygon count
            *}
            function GetPolygonCount(): NativeInt; virtual;

            {**
            * Gets frame count
            *@return frame count
            *}
            function GetFrameCount(): NativeInt; virtual;

            {**
            * Gets OpenGL command count
            *@return OpenGL command count
            *}
            function GetGlCmdCount(): NativeInt; virtual;

        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Loads MD2 from file
            *@param fileName - file name
            *@return true on success, otherwise false
            *}
            function Load(const fileName: TFileName): Boolean; override;

            {**
            * Loads MD2 from buffer
            *@param pBuffer - buffer
            *@param readLength - length to read in buffer, in bytes (not used here, can be 0)
            *@return true on success, otherwise false
            *@note Read will begin from current offset
            *}
            function Load(const pBuffer: TStream; readLength: NativeUInt): Boolean; override;

            { Properties }
            property Header:                      PQRMD2Header       read GetHeader;
            property Skins    [index: NativeInt]: PQRMD2Skin         read GetSkin;
            property TexCoords[index: NativeInt]: PQRMD2TextureCoord read GetTexCoord;
            property Polygons [index: NativeInt]: PQRMD2Polygon      read GetPolygon;
            property Frames   [index: NativeInt]: PQRMD2Frame        read GetFrame;
            property GlCmds   [index: NativeInt]: TQRInt32           read GetGlCmd;
            property SkinCount:                   NativeInt          read GetSkinCount;
            property TexCoordCount:               NativeInt          read GetTexCoordCount;
            property PolygonCount:                NativeInt          read GetPolygonCount;
            property FrameCount:                  NativeInt          read GetFrameCount;
            property GlCmdCount:                  NativeInt          read GetGlCmdCount;
    end;

    {**
    * MD2 pre-calculated light. This light is calculated while the mesh is generated, and thus is
    * embedded in the vertex buffer itself
    *}
    TQRMD2Light = class
        protected
            m_pAmbient:  TQRColor;
            m_pColor:    TQRColor;
            m_Direction: TQRVector3D;
            m_Enabled:   Boolean;

            {**
            * Gets ambient color
            *@return ambient color
            *@note The ambient color is the color of all meshes that are not illuminated by another
            *      light. This can be compared e.g. to a room where no light source are visible
            }
            function GetAmbient(): TQRColor; virtual;

            {**
            * Sets ambient color
            *@param pColor - ambient color
            *@note The ambient color is the color of all meshes that are not illuminated by another
            *      light. This can be compared e.g. to a room where no light source are visible
            }
            procedure SetAmbient(const pColor: TQRColor); virtual;

            {**
            * Gets light color
            *@return light color
            }
            function GetColor(): TQRColor; virtual;

            {**
            * Sets light color
            *@param pColor - light color
            }
            procedure SetColor(const pColor: TQRColor); virtual;

            {**
            * Gets light direction
            *@return light direction
            }
            function GetDirection(): PQRVector3D; virtual;

            {**
            * Sets light direction
            *@param pDirection - light direction
            }
            procedure SetDirection(const pDirection: PQRVector3D); virtual;

            {**
            * Gets enabled flag
            *@return enabled flag
            }
            function GetEnabled(): Boolean; virtual;

            {**
            * Sets enabled flag
            *@param value - enabled flag value to set
            }
            procedure SetEnabled(value: Boolean); virtual;

        public
            { Construction/Destruction }
            constructor Create();  virtual;
            destructor  Destroy(); override;

            {**
            * Assigns light content from another light
            *@param pOther - other light to assign from
            *}
            procedure Assign(const pOther: TQRMD2Light); virtual;

            { Properties }
            property Ambient:   TQRColor    read GetAmbient   write SetAmbient;
            property Color:     TQRColor    read GetColor     write SetColor;
            property Direction: PQRVector3D read GetDirection write SetDirection;
            property Enabled:   Boolean     read GetEnabled   write SetEnabled;
    end;

    TQRMD2Normals = array of TQRVector3D;

    {**
    * MD2 model
    *}
    TQRMD2Model = class(TQRFramedModel)
        protected
            m_pParser:             TQRMD2Parser;
            m_Normals:             TQRMD2Normals;
            m_RHToLH:              Boolean;
            m_pPreCalculatedLight: TQRMD2Light;

            {**
            * Uncompresses MD2 vertex
            *@param pFrame - MD2 frame
            *@param pVertex - MD2 vertex to uncompress
            *@return uncompressed vertex
            *}
            function UncompressVertex(const frame: TQRMD2Frame;
                                     const vertex: TQRMD2Vertex): TQRVector3D; virtual;

            {**
            * Gets vertex pre-calculated light color
            *@param pLight - MD2 pre-calculated light model to use
            *@param normal - vertex normal
            *@return vertex color
            *}
            function GetPreCalculatedLightColor(const pLight: TQRMD2Light;
                                                const normal: TQRVector3D): TQRColor; virtual;

            {**
            * Gets vertex pre-calculated light
            *@param normal - vertex normal
            *@return vertex color
            *}
            function GetPreCalculatedLight(): TQRMD2Light; virtual;

            {**
            * Sets vertex pre-calculated light
            *@param normal - vertex normal
            *@return vertex color
            *}
            procedure SetPreCalculatedLight(const light: TQRMD2Light); virtual;

            {**
            * Gets left hand to right hand conversion mode flag status
            *@return left hand to right hand conversion mode flag status
            *}
            function GetRHToLH(): Boolean; virtual;

            {**
            * Sets left hand to right hand conversion mode flag status
            *@param value - left hand to right hand conversion mode flag status to set
            *}
            procedure SetRHToLH(value: Boolean); virtual;

            {**
            * Gets MD2 parser
            *@return MD2 parser
            *}
            function GetParser(): TQRMD2Parser; virtual;

        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Loads MD2 from file
            *@param fileName - file name
            *@return true on success, otherwise false
            *}
            function Load(const fileName: TFileName): Boolean; overload; virtual;

            {**
            * Loads MD2 from buffer
            *@param pBuffer - buffer
            *@param readLength - length to read in buffer, in bytes
            *@return true on success, otherwise false
            *@note Read will begin from current offset
            *}
            function Load(const pBuffer: TStream;
                             readLength: NativeUInt): Boolean; overload; virtual;

            {**
            * Loads normals table from file
            *@param fileName - file name
            *@return true on success, otherwise false
            *}
            function LoadNormals(const fileName: TFileName): Boolean; overload; virtual;

            {**
            * Loads normals table from buffer
            *@param pBuffer - buffer
            *@param readLength - length to read in buffer, in bytes (not used here, can be 0)
            *@return true on success, otherwise false
            *}
            function LoadNormals(const pBuffer: TStream;
                                    readLength: NativeUInt): Boolean; overload; virtual;

            {**
            * Gets skin names list
            *@param pNames - skin names
            *@return true on success, otherwise false
            *@note Names list should be empty and not sorted, otherwise skin index in source parser
            *      may not match
            *}
            function GetSkinNames(const pNames: TStringList): Boolean; virtual;

            {**
            * Gets frame names list
            *@param pNames - frame names
            *@return true on success, otherwise false
            *@note Names list should be empty and not sorted, otherwise frame index in source parser
            *      may not match
            *}
            function GetFrameNames(const pNames: TStringList): Boolean; virtual;

            {**
            * Gets model frame mesh
            *@param index - frame mesh index to create
            *@param[out] mesh - frame mesh
            *@param pAABBTree - aligned-axis bounding box tree to populate, ignored if nil
            *@param hIsCanceled - callback function that allows to break the operation, can be nil
            *@return true on success, otherwise false
            *@note vertex buffer content is organized as follow:
            *      [1]x [2]y [3]z [4]nx [5]ny [6]nz [7]tu [8]tv [9]r [10]g [11]b [12]a
            *      where:
            *      x/y/z    - vertex coordinates
            *      nx/ny/nz - vertex normal (if includeNormal is activated)
            *      tu/tv    - vertex texture coordinates(if includeTexture is activated)
            *      r/g/b/a  - vertex color(if includeColor is activated)
            *}
            function GetMesh(index: NativeUInt;
                          out mesh: TQRMesh;
                         pAABBTree: TQRAABBTree;
                       hIsCanceled: TQRIsCanceledEvent = nil): Boolean; overload; override;

            {**
            * Gets model frame mesh
            *@param index - frame mesh index to get
            *@param nextIndex - frame mesh index to interpolate with
            *@param interpolationFactor - interpolation factor to apply
            *@param[out] mesh - frame mesh
            *@param hIsCanceled - callback function that allows to break the operation, can be nil
            *@return true on success, otherwise false
            *@note vertex buffer content is organized as follow:
            *      [1]x [2]y [3]z [4]nx [5]ny [6]nz [7]tu [8]tv [9]r [10]g [11]b [12]a
            *      where:
            *      x/y/z    - vertex coordinates
            *      nx/ny/nz - vertex normal (if includeNormal is activated)
            *      tu/tv    - vertex texture coordinates(if includeTexture is activated)
            *      r/g/b/a  - vertex color(if includeColor is activated)
            *}
            function GetMesh(index, nextIndex: NativeUInt;
                          interpolationFactor: Double;
                                     out mesh: TQRMesh;
                                  hIsCanceled: TQRIsCanceledEvent): Boolean; overload; override;

            {**
            * Gets mesh count
            *@return mesh count
            *}
            function GetMeshCount(): NativeUInt; override;

            { Properties }
            property PreCalculatedLight: TQRMD2Light  read GetPreCalculatedLight write SetPreCalculatedLight;
            property RHToLH:             Boolean      read GetRHToLH             write SetRHToLH;
            property Parser:             TQRMD2Parser read GetParser;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRMD2Header
//--------------------------------------------------------------------------------------------------
procedure TQRMD2Header.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD2Header), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD2 file to read next data - ' + errorMsg);

    // read header from file
    pBuffer.Read(PQRUInt32(m_ID),                 SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_Version),            SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_SkinWidth),          SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_SkinHeight),         SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_FrameSize),          SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_SkinCount),          SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_VertexCount),        SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_TextureCoordCount),  SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_PolygonCount),       SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_GlCmdsCount),        SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_FrameCount),         SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_SkinOffset),         SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_TextureCoordOffset), SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_PolygonOffset),      SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_FrameOffset),        SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_GlCmdsOffset),       SizeOf(TQRUInt32));
    pBuffer.Read(PQRUInt32(m_EndOffset),          SizeOf(TQRUInt32));
end;
//--------------------------------------------------------------------------------------------------
// TQRMD2Skin
//--------------------------------------------------------------------------------------------------
procedure TQRMD2Skin.Read(pBuffer: TStream);
var
    name:     TQRAnsiCharArray;
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, 64 * SizeOf(AnsiChar), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD2 file to read next data - ' + errorMsg);

    // reserve memory for char array
    SetLength(name, 64);

    try
        // initialize char array memory by filling it with 0
        FillChar(name[0], 64 * SizeOf(AnsiChar), $00);

        // read skin name
        pBuffer.Read(name[0], Length(name) * SizeOf(AnsiChar));

        // set skin name
        m_Name := TQRStringHelper.AnsiCharArrayToStr(name);
    finally
        // clear memory
        SetLength(name, 0);
    end;
end;
//--------------------------------------------------------------------------------------------------
// TQRMD2Vertex
//--------------------------------------------------------------------------------------------------
procedure TQRMD2Vertex.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD2Vertex), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD2 file to read next data - ' + errorMsg);

    // read vertex from file
    pBuffer.Read(m_Vertex[0],   SizeOf(m_Vertex));
    pBuffer.Read(m_NormalIndex, SizeOf(TQRUInt8));
end;
//--------------------------------------------------------------------------------------------------
// TQRMD2TextureCoord
//--------------------------------------------------------------------------------------------------
procedure TQRMD2TextureCoord.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD2TextureCoord), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD2 file to read next data - ' + errorMsg);

    // read texture coordinates from file
    pBuffer.Read(m_U, SizeOf(TQRInt16));
    pBuffer.Read(m_V, SizeOf(TQRInt16));
end;
//--------------------------------------------------------------------------------------------------
// TQRMD2Frame
//--------------------------------------------------------------------------------------------------
procedure TQRMD2Frame.Read(pBuffer: TStream; const header: TQRMD2Header);
var
    dataLength: NativeUInt;
    name:       TQRAnsiCharArray;
    i:          Cardinal;
    errorMsg:   UnicodeString;
begin
    // calculate needed length to read next data in buffer
    dataLength := (16 * SizeOf(AnsiChar)) + SizeOf(m_Scale) + SizeOf(m_Translate);

    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, dataLength, errorMsg)) then
        raise Exception.Create('Not enough bytes in MD2 file to read next data - ' + errorMsg);

    if (Length(m_Vertex) > 0) then
        SetLength(m_Vertex, 0);

    // read vertex transformations
    pBuffer.Read(m_Scale,     SizeOf(m_Scale));
    pBuffer.Read(m_Translate, SizeOf(m_Translate));

    // reserve memory for char array
    SetLength(name, 16);

    try
        // initialize char array memory filling it with 0
        FillChar(name[0], 16 * SizeOf(AnsiChar), $00);

        // read frame name
        pBuffer.Read(name[0], Length(name) * SizeOf(AnsiChar));

        // set frame name
        m_Name := TQRStringHelper.AnsiCharArrayToStr(name);
    finally
        // clear memory
        SetLength(name, 0);
    end;

    // no vertex?
    if (header.m_VertexCount = 0) then
        Exit;

    // create frame vertex buffer
    SetLength(m_Vertex, header.m_VertexCount);

    // read frame vertices
    for i := 0 to header.m_VertexCount - 1 do
        m_Vertex[i].Read(pBuffer);
end;
//--------------------------------------------------------------------------------------------------
// TQRMD2Polygon
//--------------------------------------------------------------------------------------------------
procedure TQRMD2Polygon.Read(pBuffer: TStream);
var
    errorMsg: UnicodeString;
begin
    // check if buffer is large enough to read next data block
    if (not TQRModelHelper.ValidateNextRead(pBuffer, SizeOf(TQRMD2Polygon), errorMsg)) then
        raise Exception.Create('Not enough bytes in MD2 file to read next data - ' + errorMsg);

    // read polygon from file
    pBuffer.Read(m_VertexIndex,       SizeOf(m_VertexIndex));
    pBuffer.Read(m_TextureCoordIndex, SizeOf(m_TextureCoordIndex));
end;
//--------------------------------------------------------------------------------------------------
// TQRMD2Parser
//--------------------------------------------------------------------------------------------------
constructor TQRMD2Parser.Create();
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD2Parser.Destroy();
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Parser.GetHeader(): PQRMD2Header;
begin
    Result := @m_Header;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Parser.GetSkin(index: NativeInt): PQRMD2Skin;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_Skins))) then
    begin
        Result := nil;
        Exit;
    end;

    Result := @m_Skins[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Parser.GetTexCoord(index: NativeInt): PQRMD2TextureCoord;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_TexCoords))) then
    begin
        Result := nil;
        Exit;
    end;

    Result := @m_TexCoords[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Parser.GetPolygon(index: NativeInt): PQRMD2Polygon;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_Polygons))) then
    begin
        Result := nil;
        Exit;
    end;

    Result := @m_Polygons[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Parser.GetFrame(index: NativeInt): PQRMD2Frame;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_Frames))) then
    begin
        Result := nil;
        Exit;
    end;

    Result := @m_Frames[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Parser.GetGlCmd(index: NativeInt): TQRInt32;
begin
    // is index out of bounds?
    if ((index < 0) or (index >= Length(m_GlCmds))) then
    begin
        Result := -1;
        Exit;
    end;

    Result := m_GlCmds[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Parser.GetSkinCount(): NativeInt;
begin
    Result := Length(m_Skins);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Parser.GetTexCoordCount(): NativeInt;
begin
    Result := Length(m_TexCoords);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Parser.GetPolygonCount(): NativeInt;
begin
    Result := Length(m_Polygons);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Parser.GetFrameCount(): NativeInt;
begin
    Result := Length(m_Frames);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Parser.GetGlCmdCount(): NativeInt;
begin
    Result := Length(m_GlCmds);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Parser.Load(const fileName: TFileName): Boolean;
var
    pBuffer: TFileStream;
begin
    pBuffer := nil;

    try
        // file exists?
        if (not FileExists(fileName)) then
        begin
            Result := False;
            Exit;
        end;

        // create a file buffer and open it for read
        pBuffer := TFileStream.Create(fileName, fmOpenRead);
        pBuffer.Seek(0, soBeginning);

        // read MD2 content
        Result := Load(pBuffer, pBuffer.Size);
    finally
        // clear buffer
        pBuffer.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Parser.Load(const pBuffer: TStream; readLength: NativeUInt): Boolean;
var
    offset: NativeUInt;
    i:      NativeUInt;
begin
    try
        // is buffer empty?
        if (pBuffer.Size = 0) then
        begin
            Result := False;
            Exit;
        end;

        // get current offset
        offset := pBuffer.Position;

        // read file header
        m_Header.Read(pBuffer);

        // is MD2 file and version correct?
        if ((m_Header.m_ID <> CQR_MD2_ID) or (m_Header.m_Version <> CQR_MD2_Mesh_File_Version)) then
        begin
            Result := False;
            Exit;
        end;

        // create mesh buffers
        SetLength(m_Skins,     m_Header.m_SkinCount);
        SetLength(m_TexCoords, m_Header.m_TextureCoordCount);
        SetLength(m_Polygons,  m_Header.m_PolygonCount);
        SetLength(m_GlCmds,    m_Header.m_GlCmdsCount);
        SetLength(m_Frames,    m_Header.m_FrameCount);

        // read skins
        if (m_Header.m_SkinCount > 0) then
        begin
            // go to skins offset
            pBuffer.Seek(offset + m_Header.m_SkinOffset, soBeginning);

            for i := 0 to m_Header.m_SkinCount - 1 do
                m_Skins[i].Read(pBuffer);
        end;

        // read texture coordinates
        if (m_Header.m_TextureCoordCount > 0) then
        begin
            // go to texture coordinates offset
            pBuffer.Seek(offset + m_Header.m_TextureCoordOffset, soBeginning);

            for i := 0 to m_Header.m_TextureCoordCount - 1 do
                m_TexCoords[i].Read(pBuffer);
        end;

        // read polygons
        if (m_Header.m_PolygonCount > 0) then
        begin
            // go to polygons offset
            pBuffer.Seek(offset + m_Header.m_PolygonOffset, soBeginning);

            for i := 0 to m_Header.m_PolygonCount - 1 do
                m_Polygons[i].Read(pBuffer);
        end;

        // read OpenGL commands
        if (m_Header.m_GlCmdsCount > 0) then
        begin
            pBuffer.Seek(offset + m_Header.m_GlCmdsOffset, soBeginning);
            pBuffer.Read(m_GlCmds[0], SizeOf(TQRInt32) * m_Header.m_GlCmdsCount);
        end;

        // read frames
        if (m_Header.m_FrameCount > 0) then
        begin
            // go to frames offset
            pBuffer.Seek(offset + m_Header.m_FrameOffset, soBeginning);

            for i := 0 to m_Header.m_FrameCount - 1 do
                m_Frames[i].Read(pBuffer, m_Header);
        end;

        Result := True;
        Exit;
    except
        on e: Exception do ; // ignore any error
    end;

    Result := False;
end;
//--------------------------------------------------------------------------------------------------
// TQRMD2Light
//--------------------------------------------------------------------------------------------------
constructor TQRMD2Light.Create();
begin
    inherited Create;

    m_pAmbient  := TQRColor.Create(0, 0, 0);
    m_pColor    := TQRColor.Create(0, 0, 0);
    m_Direction := TQRVector3D.Create(0.0, 0.0, 0.0);
    m_Enabled   := False;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD2Light.Destroy();
begin
    m_pAmbient.Free;
    m_pColor.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Light.GetAmbient(): TQRColor;
begin
    Result := m_pAmbient;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD2Light.SetAmbient(const pColor: TQRColor);
begin
    m_pAmbient.Assign(pColor);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Light.GetColor(): TQRColor;
begin
    Result := m_pColor;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD2Light.SetColor(const pColor: TQRColor);
begin
    m_pColor.Assign(pColor);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Light.GetDirection(): PQRVector3D;
begin
    Result := @m_Direction;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD2Light.SetDirection(const pDirection: PQRVector3D);
begin
    m_Direction := pDirection^;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Light.GetEnabled(): Boolean;
begin
    Result := m_Enabled;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD2Light.SetEnabled(value: Boolean);
begin
    m_Enabled := value;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD2Light.Assign(const pOther: TQRMD2Light);
begin
    // copy light from other
    m_pAmbient.Assign(pOther.m_pAmbient);
    m_pColor.Assign(pOther.m_pColor);
    m_Direction := pOther.m_Direction;
    m_Enabled   := pOther.m_Enabled;
end;
//--------------------------------------------------------------------------------------------------
// TQRMD2Model
//--------------------------------------------------------------------------------------------------
constructor TQRMD2Model.Create();
begin
    inherited Create;

    m_pParser             := TQRMD2Parser.Create();
    m_pPreCalculatedLight := TQRMD2Light.Create;
    m_RHToLH              := False;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD2Model.Destroy();
begin
    // clear memory
    SetLength(m_Normals, 0);
    m_pPreCalculatedLight.Free;
    m_pParser.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Model.UncompressVertex(const frame: TQRMD2Frame;
                                     const vertex: TQRMD2Vertex): TQRVector3D;
var
    vertArray: array[0..2] of Single;
    i:         Byte;
begin
    // iterate through vertex coordinates
    for i := 0 to 2 do
        // uncompress vertex using frame scale and translate values
        vertArray[i] := (frame.m_Scale[i] * vertex.m_Vertex[i]) + frame.m_Translate[i];

    Result := TQRVector3D.Create(vertArray[0], vertArray[1], vertArray[2]);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Model.GetPreCalculatedLightColor(const pLight: TQRMD2Light;
                                                const normal: TQRVector3D): TQRColor;
var
    lightAngle: Single;
    r, g, b, a: Cardinal;
begin
    // calculate light angle
    lightAngle := normal.Dot(pLight.Direction^);

    // is light angle out of bounds? (necessary to ensure that the light is not calculated on the 2
    // sides of the vertex, and thus give the impression that the light comes from two opposite
    // sides at once)
    if (lightAngle < 0.0) then
        lightAngle := 0.0;

    // calculate light color
    r := Floor(Min(255.0, Max((pLight.Color.GetRed()   * lightAngle), pLight.Ambient.GetRed())));
    g := Floor(Min(255.0, Max((pLight.Color.GetGreen() * lightAngle), pLight.Ambient.GetGreen())));
    b := Floor(Min(255.0, Max((pLight.Color.GetBlue()  * lightAngle), pLight.Ambient.GetBlue())));
    a := Floor(Min(255.0, Max((pLight.Color.GetAlpha() * lightAngle), pLight.Ambient.GetAlpha())));

    Result := TQRColor.Create(PByte(@r)^, PByte(@g)^, PByte(@b)^, PByte(@a)^);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Model.GetPreCalculatedLight(): TQRMD2Light;
begin
    Result := m_pPreCalculatedLight;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD2Model.SetPreCalculatedLight(const light: TQRMD2Light);
begin
    m_pPreCalculatedLight.Assign(light);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Model.GetRHToLH(): Boolean;
begin
    Result := m_RHToLH;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD2Model.SetRHToLH(value: Boolean);
begin
    m_RHToLH := value;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Model.GetParser(): TQRMD2Parser;
begin
    Result := m_pParser;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Model.Load(const fileName: TFileName): Boolean;
begin
    Result := m_pParser.Load(fileName);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Model.Load(const pBuffer: TStream; readLength: NativeUInt): Boolean;
begin
    Result := m_pParser.Load(pBuffer, readLength);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Model.LoadNormals(const fileName: TFileName): Boolean;
var
    pBuffer: TFileStream;
begin
    pBuffer := nil;

    try
        // file exists?
        if (not FileExists(fileName)) then
        begin
            Result := False;
            Exit;
        end;

        // create a file buffer and open it for read
        pBuffer := TFileStream.Create(fileName, fmOpenRead);
        pBuffer.Seek(0, soBeginning);

        // read MD2 normals file content
        Result := LoadNormals(pBuffer, pBuffer.Size);
    finally
        // clear buffer
        pBuffer.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Model.LoadNormals(const pBuffer: TStream; readLength: NativeUInt): Boolean;
var
    fileVersion, x, y, z: Single;
    dataLength:           TQRUInt32;
    i:                    Cardinal;
begin
    // clear previous table, if needed
    SetLength(m_Normals, 0);

    // is buffer empty?
    if (pBuffer.Size = 0) then
    begin
        Result := False;
        Exit;
    end;

    // read version
    pBuffer.ReadData(fileVersion);

    // is version correct?
    if (fileVersion <> CQR_MD2_Normals_Table_File_Version) then
    begin
        Result := False;
        Exit;
    end;

    // read file length
    pBuffer.ReadData(dataLength);

    // is file empty?
    if (dataLength = 0) then
    begin
        Result := False;
        Exit;
    end;

    // reserve memory for normals table
    SetLength(m_Normals, dataLength);

    // iterate through normals
    for i := 0 to dataLength - 1 do
    begin
        // read next normal from file
        pBuffer.ReadData(x);
        pBuffer.ReadData(y);
        pBuffer.ReadData(z);

        // set normal in table
        m_Normals[i] := TQRVector3D.Create(x, y, z);
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Model.GetSkinNames(const pNames: TStringList): Boolean;
var
    skinCount, i: NativeInt;
begin
    if (not Assigned(pNames)) then
    begin
        Result := False;
        Exit;
    end;

    // get skin count
    skinCount := Length(m_pParser.m_Skins);

    // no skin?
    if (skinCount = 0) then
    begin
        Result := False;
        Exit;
    end;

    // iterate through source skins and get each name
    for i := 0 to skinCount - 1 do
        pNames.Add(UnicodeString(m_pParser.m_Skins[i].m_Name));

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Model.GetFrameNames(const pNames: TStringList): Boolean;
var
    frameCount, i: NativeInt;
begin
    // get frame count
    frameCount := Length(m_pParser.m_Frames);

    // no frame?
    if (frameCount = 0) then
    begin
        Result := False;
        Exit;
    end;

    // iterate through source frames and get each name
    for i := 0 to frameCount - 1 do
        pNames.Add(UnicodeString(m_pParser.m_Frames[i].m_Name));

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Model.GetMesh(index: NativeUInt;
                          out mesh: TQRMesh;
                         pAABBTree: TQRAABBTree;
                       hIsCanceled: TQRIsCanceledEvent): Boolean;
type
    TQRSingleArray = array of Single;
var
    srcFrame:                                    TQRMD2Frame;
    stride, normalCount, i, j, meshIndex, glCmd: NativeInt;
    srcVertex:                                   TQRMD2Vertex;
    ucpVertex, normal:                           TQRVector3D;
    tu, tv:                                      Single;
    pMeshColor:                                  TQRColor;
    foundNormal, doFreeColor:                    Boolean;
    glCmdsSingle:                                TQRSingleArray;
begin
    // is frame index out of bounds?
    if (index >= GetMeshCount) then
    begin
        Result := False;
        Exit;
    end;

    // get normal count
    normalCount := Length(m_Normals);

    // do use m_Normals and pre-calculated m_Normals table wasn't populated?
    if ((EQR_VF_Normals in m_VertexFormat) and (normalCount = 0)) then
    begin
        Result := False;
        Exit;
    end;

    // get source frame from which mesh should be extracted
    srcFrame := m_pParser.m_Frames[index];

    // basically stride is the coordinates values size
    stride := 3;

    // do include m_Normals?
    if (EQR_VF_Normals in m_VertexFormat) then
        Inc(stride, 3);

    // do include texture coordinates?
    if (EQR_VF_TexCoords in m_VertexFormat) then
        Inc(stride, 2);

    // do include colors?
    if (EQR_VF_Colors in m_VertexFormat) then
        Inc(stride, 4);

    i      := 0;
    glCmd  := m_pParser.m_GLCmds[i];

    // iterate through OpenGL commands (negative value is for triangle fan,
    // positive value is for triangle strip, 0 means list end)
    while (glCmd <> 0) do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
        begin
            Result := False;
            Exit;
        end;

        // the first command is the number of vertices to process, already read, so skip it
        Inc(i);

        // add mesh to output
        SetLength(mesh, Length(mesh) + 1);
        meshIndex := Length(mesh) - 1;

        // create and populate new vertex for the current command
        mesh[meshIndex].m_Name      := 'qr_md2';
        mesh[meshIndex].m_Stride    := stride;
        mesh[meshIndex].m_Format    := m_VertexFormat;
        mesh[meshIndex].m_CoordType := EQR_VC_XYZ;

        // search for OpenGL command type
        if (glCmd < 0) then
        begin
            mesh[meshIndex].m_Type := EQR_VT_TriangleFan;
            glCmd                  := -glCmd;
        end
        else
            mesh[meshIndex].m_Type := EQR_VT_TriangleStrip;

        j := 0;

        // iterate through OpenGL commands to process
        while (glCmd > 0) do
        begin
            // is canceled?
            if (Assigned(hIsCanceled) and hIsCanceled) then
            begin
                Result := False;
                Exit;
            end;

            // get source vertex
            srcVertex := srcFrame.m_Vertex[m_pParser.m_GLCmds[i + 2]];

            // uncompress vertex
            ucpVertex := UncompressVertex(srcFrame, srcVertex);

            // do convert right hand <-> left hand coordinate system?
            if (m_RHToLH) then
                // apply conversion
                ucpVertex.X := -ucpVertex.X;

            // populate vertex buffer
            SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 3);
            mesh[meshIndex].m_Buffer[j]     := ucpVertex.X;
            mesh[meshIndex].m_Buffer[j + 1] := ucpVertex.Y;
            mesh[meshIndex].m_Buffer[j + 2] := ucpVertex.Z;
            Inc(j, 3);

            // do include m_Normals?
            if (EQR_VF_Normals in m_VertexFormat) then
            begin
                // is normal index out of bounds?
                if (srcVertex.m_NormalIndex >= normalCount) then
                begin
                    Result := False;
                    Exit;
                end;

                // get vertex normal
                normal := m_Normals[srcVertex.m_NormalIndex];

                // do convert right hand <-> left hand coordinate system?
                if (m_RHToLH) then
                    // apply conversion
                    normal.X := -normal.X;

                SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 3);
                mesh[meshIndex].m_Buffer[j]     := normal.X;
                mesh[meshIndex].m_Buffer[j + 1] := normal.Y;
                mesh[meshIndex].m_Buffer[j + 2] := normal.Z;
                Inc(j, 3);
            end;

            // do include texture coordinates?
            if (EQR_VF_TexCoords in m_VertexFormat) then
            begin
                // reinterpret OpenGL commands array as an array of single
                glCmdsSingle := TQRSingleArray(m_pParser.m_GLCmds);

                // get vertex texture coordinates
                tu := glCmdsSingle[i];
                tv := glCmdsSingle[i + 1];

                SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 2);
                mesh[meshIndex].m_Buffer[j]     := tu;
                mesh[meshIndex].m_Buffer[j + 1] := tv;
                Inc(j, 2);
            end;

            // do include colors?
            if (EQR_VF_Colors in m_VertexFormat) then
            begin
                doFreeColor := False;

                // do pre-calculate lightining or use the material color?
                if (m_pPreCalculatedLight.Enabled) then
                begin
                    // by default, normal exists
                    foundNormal := True;

                    // are normals also included?
                    if (not(EQR_VF_Normals in m_VertexFormat)) then
                    begin
                        // is normal index out of bounds?
                        if (srcVertex.m_NormalIndex >= normalCount) then
                        begin
                            // normal isn't available
                            foundNormal := False;
                        end
                        else
                        begin
                            // get vertex normal
                            normal := m_Normals[srcVertex.m_NormalIndex];

                            // do convert right hand <-> left hand coordinate system?
                            if (m_RHToLH) then
                                // apply conversion
                                normal.X := -normal.X;
                        end;
                    end;

                    // calculate lightning from pre-calculated light
                    if (foundNormal) then
                    begin
                        doFreeColor := True;
                        pMeshColor  := GetPreCalculatedLightColor(m_pPreCalculatedLight, normal);
                    end
                    else
                        // unfortunately normal isn't available, so use the ambient color (not the
                        // best solution, but for lack of better...)
                        pMeshColor := m_pPreCalculatedLight.Ambient;
                end
                else
                begin
                    // use default color
                    pMeshColor := m_pColor;
                end;

                SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 4);
                mesh[meshIndex].m_Buffer[j]     := pMeshColor.GetRedF();
                mesh[meshIndex].m_Buffer[j + 1] := pMeshColor.GetGreenF();
                mesh[meshIndex].m_Buffer[j + 2] := pMeshColor.GetBlueF();
                mesh[meshIndex].m_Buffer[j + 3] := pMeshColor.GetAlphaF();
                Inc(j, 4);

                if (doFreeColor) then
                    pMeshColor.Free;
            end;

            Dec(glCmd);
            Inc(i, 3);
        end;

        // go to next OpenGL command
        glCmd := m_pParser.m_GLCmds[i];
    end;

    // canceled?
    if (Assigned(hIsCanceled) and hIsCanceled) then
    begin
        Result := True;
        Exit;
    end;

    // no aligned-axis bounding box tree to populate?
    if (not Assigned(pAABBTree)) then
    begin
        Result := True;
        Exit;
    end;

    // populate aligned-axis bounding box tree
    Result := TQRModelHelper.PopulateAABBTree(mesh, pAABBTree, hIsCanceled);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Model.GetMesh(index, nextIndex: NativeUInt;
                          interpolationFactor: Double;
                                     out mesh: TQRMesh;
                                  hIsCanceled: TQRIsCanceledEvent): Boolean;
type
    TQRSingleArray = array of Single;
var
    srcFrame, intFrame:                                                   TQRMD2Frame;
    stride, normalCount, i, j, meshIndex, glCmd:                          NativeInt;
    srcVertex, intVertex:                                                 TQRMD2Vertex;
    ucpVertex, ucpIntVertex, finalVertex, normal, intNormal, finalNormal: TQRVector3D;
    tu, tv:                                                               Single;
    pMeshColor:                                                           TQRColor;
    foundNormal, doFreeColor:                                             Boolean;
    glCmdsSingle:                                                         TQRSingleArray;
begin
    // is frame index out of bounds?
    if ((index >= GetMeshCount) or (nextIndex >= GetMeshCount)) then
    begin
        Result := False;
        Exit;
    end;

    // get normal count
    normalCount := Length(m_Normals);

    // do use m_Normals and pre-calculated m_Normals table wasn't populated?
    if ((EQR_VF_Normals in m_VertexFormat) and (normalCount = 0)) then
    begin
        Result := False;
        Exit;
    end;

    // get source frame from which mesh should be extracted, and frame to interpolate with
    srcFrame := m_pParser.m_Frames[index];
    intFrame := m_pParser.m_Frames[nextIndex];

    // basically stride is the coordinates values size
    stride := 3;

    // do include m_Normals?
    if (EQR_VF_Normals in m_VertexFormat) then
        Inc(stride, 3);

    // do include texture coordinates?
    if (EQR_VF_TexCoords in m_VertexFormat) then
        Inc(stride, 2);

    // do include colors?
    if (EQR_VF_Colors in m_VertexFormat) then
        Inc(stride, 4);

    i      := 0;
    glCmd  := m_pParser.m_GLCmds[i];

    // iterate through OpenGL commands (negative value is for triangle fan,
    // positive value is for triangle strip, 0 means list end)
    while (glCmd <> 0) do
    begin
        // is canceled?
        if (Assigned(hIsCanceled) and hIsCanceled) then
        begin
            Result := False;
            Exit;
        end;

        // the first command is the number of vertices to process, already read, so skip it
        Inc(i);

        // add mesh to output
        SetLength(mesh, Length(mesh) + 1);
        meshIndex := Length(mesh) - 1;

        // create and populate new vertex for the current command
        mesh[meshIndex].m_Name      := 'qr_md2';
        mesh[meshIndex].m_Stride    := stride;
        mesh[meshIndex].m_Format    := m_VertexFormat;
        mesh[meshIndex].m_CoordType := EQR_VC_XYZ;

        // search for OpenGL command type
        if (glCmd < 0) then
        begin
            mesh[meshIndex].m_Type := EQR_VT_TriangleFan;
            glCmd                  := -glCmd;
        end
        else
            mesh[meshIndex].m_Type := EQR_VT_TriangleStrip;

        j := 0;

        // iterate through OpenGL commands to process
        while (glCmd > 0) do
        begin
            // is canceled?
            if (Assigned(hIsCanceled) and hIsCanceled) then
            begin
                Result := False;
                Exit;
            end;

            // get source vertex, and vertex to interpolate with
            srcVertex := srcFrame.m_Vertex[m_pParser.m_GLCmds[i + 2]];
            intVertex := intFrame.m_Vertex[m_pParser.m_GLCmds[i + 2]];

            // uncompress vertices
            ucpVertex    := UncompressVertex(srcFrame, srcVertex);
            ucpIntVertex := UncompressVertex(intFrame, intVertex);

            // do convert right hand <-> left hand coordinate system?
            if (m_RHToLH) then
            begin
                // apply conversion
                ucpVertex.X    := -ucpVertex.X;
                ucpIntVertex.X := -ucpIntVertex.X;
            end;

            // calculate final mesh vertex
            finalVertex := ucpVertex.Interpolate(ucpIntVertex, interpolationFactor);

            // populate vertex buffer
            SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 3);
            mesh[meshIndex].m_Buffer[j]     := finalVertex.X;
            mesh[meshIndex].m_Buffer[j + 1] := finalVertex.Y;
            mesh[meshIndex].m_Buffer[j + 2] := finalVertex.Z;
            Inc(j, 3);

            // do include m_Normals?
            if (EQR_VF_Normals in m_VertexFormat) then
            begin
                // is normal index out of bounds?
                if (srcVertex.m_NormalIndex >= normalCount) then
                begin
                    Result := False;
                    Exit;
                end;

                // get vertex normal, and normal to interpolate with
                normal    := m_Normals[srcVertex.m_NormalIndex];
                intNormal := m_Normals[srcVertex.m_NormalIndex];

                // do convert right hand <-> left hand coordinate system?
                if (m_RHToLH) then
                begin
                    // apply conversion
                    normal.X    := -normal.X;
                    intNormal.X := -intNormal.X;
                end;

                // calculate final vertex normal
                finalNormal := normal.Interpolate(intNormal, interpolationFactor);

                SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 3);
                mesh[meshIndex].m_Buffer[j]     := finalNormal.X;
                mesh[meshIndex].m_Buffer[j + 1] := finalNormal.Y;
                mesh[meshIndex].m_Buffer[j + 2] := finalNormal.Z;
                Inc(j, 3);
            end;

            // do include texture coordinates?
            if (EQR_VF_TexCoords in m_VertexFormat) then
            begin
                // reinterpret OpenGL commands array as an array of single
                glCmdsSingle := TQRSingleArray(m_pParser.m_GLCmds);

                // get vertex texture coordinates
                tu := glCmdsSingle[i];
                tv := glCmdsSingle[i + 1];

                SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 2);
                mesh[meshIndex].m_Buffer[j]     := tu;
                mesh[meshIndex].m_Buffer[j + 1] := tv;
                Inc(j, 2);
            end;

            // do include colors?
            if (EQR_VF_Colors in m_VertexFormat) then
            begin
                doFreeColor := False;

                // do pre-calculate lightining or use the material color?
                if (m_pPreCalculatedLight.Enabled) then
                begin
                    // by default, normal exists
                    foundNormal := True;

                    // are normals also included?
                    if (not(EQR_VF_Normals in m_VertexFormat)) then
                    begin
                        // is normal index out of bounds?
                        if (srcVertex.m_NormalIndex >= normalCount) then
                        begin
                            // normal isn't available
                            foundNormal := False;
                        end
                        else
                        begin
                            // get vertex normal
                            normal := m_Normals[srcVertex.m_NormalIndex];

                            // do convert right hand <-> left hand coordinate system?
                            if (m_RHToLH) then
                                // apply conversion
                                normal.X := -normal.X;
                        end;
                    end;

                    // calculate lightning from pre-calculated light
                    if (foundNormal) then
                    begin
                        doFreeColor := True;
                        pMeshColor   := GetPreCalculatedLightColor(m_pPreCalculatedLight, normal);
                    end
                    else
                        // unfortunately normal isn't available, so use the ambient color (not the
                        // best solution, but for lack of better...)
                        pMeshColor := m_pPreCalculatedLight.Ambient;
                end
                else
                begin
                    // use default color
                    pMeshColor := m_pColor;
                end;

                SetLength(mesh[meshIndex].m_Buffer, Length(mesh[meshIndex].m_Buffer) + 4);
                mesh[meshIndex].m_Buffer[j]     := pMeshColor.GetRedF();
                mesh[meshIndex].m_Buffer[j + 1] := pMeshColor.GetGreenF();
                mesh[meshIndex].m_Buffer[j + 2] := pMeshColor.GetBlueF();
                mesh[meshIndex].m_Buffer[j + 3] := pMeshColor.GetAlphaF();
                Inc(j, 4);

                if (doFreeColor) then
                    pMeshColor.Free;
            end;

            Dec(glCmd);
            Inc(i, 3);
        end;

        // go to next OpenGL command
        glCmd := m_pParser.m_GLCmds[i];
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD2Model.GetMeshCount(): NativeUInt;
begin
    Result := m_pParser.m_Header.m_FrameCount;
end;
//--------------------------------------------------------------------------------------------------

end.
