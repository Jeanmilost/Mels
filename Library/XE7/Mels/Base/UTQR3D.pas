{**************************************************************************************************
 * ==> UTQR3D ------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides the basic classes to work with 3D worlds and models         *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQR3D;

interface

uses System.Classes,
     System.SysUtils,
     UTQRGeometry;

type
    {**
    * Vertex formats enumeration
    *}
    EQRVertexFormats =
    (
        EQR_VF_Normals,
        EQR_VF_TexCoords,
        EQR_VF_Colors
    );

    {**
    * Vertex formats set
    *}
    TQRVertexFormat = set of EQRVertexFormats;

    {**
    * Vertex buffer type enumeration
    *}
    EQRVertexType =
    (
        EQR_VT_Unknown = 0,
        EQR_VT_Triangles,
        EQR_VT_TriangleStrip,
        EQR_VT_TriangleFan,
        EQR_VT_Quads,
        EQR_VT_QuadStrip
    );

    {**
    * Vertex coordinate type
    *}
    EQRVertexCoordType =
    (
        EQR_VC_Unknown = 0,
        EQR_VC_XY,
        EQR_VC_XYZ
    );

    TQRVertexBuffer = array of Single;

    {**
    * Vertex descriptor, contains global enumeration and types
    *}
    TQRVertex = record
        public
            m_Name:      UnicodeString;      // optional name, used e.g. to link the vertex with a texture
            m_Stride:    NativeUInt;         // vertex stride (i.e. length between each vertex) in bytes
            m_Type:      EQRVertexType;      // vertex type (i.e. how vertex is organized: triangle list, triangle fan, ...)
            m_Format:    TQRVertexFormat;    // vertex format (i.e. what data vertex contains: position, normal, texture, ...)
            m_CoordType: EQRVertexCoordType; // vertex coordinate type (i.e. 2D coordinates, 3D coordinates, ...)
            m_Buffer:    TQRVertexBuffer;    // vertex buffer

            {**
            * Gets record initialized with default values
            *@return record initialized with default values
            *}
            class function GetDefault: TQRVertex; inline; static;

            {**
            * Clears record
            *}
            procedure Clear(); inline;

            {**
            * Clones vertex in such manner that vertex info are copied, but not vertex buffer
            *@return cloned vertex
            *@note Cloned vertex should be deleted when useless
            *}
            function Clone(): TQRVertex; inline;

            {**
            * Calculates vertex stride
            *@return vertex stride
            *}
            function CalculateStride(): NativeUInt;

            {**
            * Compares vertex and determine if their format are equivalent
            *@param other - other vertex to compare with
            *@return true if both vertex header are equivalent, otherwise false
            *}
            function CompareFormat(const other: TQRVertex): Boolean; inline;
    end;

    PQRVertex = ^TQRVertex;

    {**
    * Mesh, it's a set of vertex buffers representing a model
    *}
    TQRMesh = array of TQRVertex;
    PQRMesh = ^TQRMesh;

    {**
    * Shader type enumeration
    *}
    EQRShaderType =
    (
        EQR_ST_Vertex,
        EQR_ST_Fragment
    );

    {**
    * Shader attributes
    *}
    EQRShaderAttribute =
    (
        EQR_SA_Position,
        EQR_SA_Normal,
        EQR_SA_Texture,
        EQR_SA_Color,
        EQR_SA_PerspectiveMatrix,
        EQR_SA_ProjectionMatrix, // normally the same as perspective matrix, just for name convention
        EQR_SA_ViewMatrix,
        EQR_SA_CameraMatrix,     // normally the same as view matrix, just for name convention
        EQR_SA_ModelMatrix,
        EQR_SA_Interpolation,
        EQR_SA_InterpolationPos,
        EQR_SA_InterpolationNormal,
        EQR_SA_ColorMap
    );

    TQRAttributeDictionary = TStringList;

    {**
    * Basic shader language class
    *}
    TQRShader = class
        protected
            m_pAttributeDictionary: TQRAttributeDictionary;

            {**
            * Populates default attribute dictionary
            *}
            procedure PopulateAttributeDict(); virtual;

        public
            { Construction/Destruction }
            constructor Create();  virtual;
            destructor  Destroy(); override;

            {**
            * Gets attribute name
            *@param attribute - attribute to get name
            *@return attribute name
            *}
            function GetAttributeName(attribute: EQRShaderAttribute): UnicodeString; virtual;

            {**
            * Sets attribute name
            *@param attribute - attribute to set name
            *@param name - new attribute name
            *}
            procedure SetAttributeName(attribute: EQRShaderAttribute;
                                      const name: UnicodeString); virtual;

            {**
            * Gets shader program identifier
            *@return shader program identifier
            *}
            function GetProgramID(): NativeUInt; virtual; abstract;

            {**
            * Attaches shader to program from stream
            *@param pStream - stream containing shader
            *@param type - shader type
            *@return compiled shader identifier
            *}
            function AttachFile(pStream: TStream;
                             shaderType: EQRShaderType): NativeUInt; overload; virtual; abstract;

            {**
            * Attaches shader to program from file
            *@param fileName - shader file
            *@param type - shader type
            *@return compiled shader identifier
            *}
            function AttachFile(const fileName: TFileName;
                                    shaderType: EQRShaderType): NativeUInt; overload; virtual; abstract;

            {**
            * Attaches shader to program
            *@param source - shader source code
            *@param type - shader type
            *@return compiled shader identifier
            *}
            function Attach(const source: UnicodeString;
                              shaderType: EQRShaderType): NativeUInt; virtual; abstract;

            {**
            * Links all attached shader and keep program ready to run
            *@param use - if true, program will be used immediately (in case link succeeded)
            *@return true on success, otherwise false
            *}
            function Link(use: Boolean): Boolean; virtual; abstract;

            {**
            * Uses the program
            *@param use - if true, program will be used, released otherwise
            *}
            procedure Use(use: Boolean); virtual; abstract;
    end;

    {**
    * Rotation info, describe a rotation to apply e.g. to a model
    *}
    TQRRotation = class
        protected
            m_Angle: Single;
            m_Axis:  TQRVector3D;

            {**
            * Gets angle
            *@return angle
            *}
            function GetAngle(): Single; virtual;

            {**
            * Sets angle
            *@param angle - angle
            *}
            procedure SetAngle(angle: Single); virtual;

            {**
            * Gets axis
            *@return axis
            *}
            function GetAxis(): PQRVector3D; virtual;

            {**
            * Sets axis
            *@param pAxis - axis
            *}
            procedure SetAxis(pAxis: PQRVector3D); virtual;

        public
            {**
            * Constructor
            *}
            constructor Create(); overload; virtual;

            {**
            * Constructor
            *@param angle - rotation angle in radians
            *@param axis - rotation axis, e.g. [0.0, 1.0, 0.0] for a rotation on the y axis
            *}
            constructor Create(const angle: Single; const axis: TQRVector3D); overload; virtual;

            {**
            * Destructor
            *}
            destructor Destroy(); override;

            { Properties }
            property Angle: Single      read GetAngle write SetAngle;
            property Axis:  PQRVector3D read GetAxis  write SetAxis;
    end;

    {**
    * Model texture wrap mode enumeration
    *}
    EQRTextureWrapMode =
    (
        EQR_WM_CLAMP_TO_EDGE,
        EQR_WM_CLAMP_TO_BORDER,
        EQR_WM_MIRRORED_REPEAT,
        EQR_WM_REPEAT,
        EQR_WM_MIRROR_CLAMP_TO_EDGE
    );

    {**
    * Texture coordinate
    *}
    TQRTexCoord = record
        private
            m_U: Single;
            m_V: Single;

        public
            { Properties }
            property U: Single read m_U write m_U;
            property V: Single read m_V write m_V;
    end;

    PQRTexCoord = ^TQRTexCoord;

    {**
    * Texture info, contains all info about a texture item
    *}
    TQRTexture = class
        protected
            m_Index:       NativeUInt;
            m_Name:        UnicodeString;
            m_Dir:         UnicodeString;
            m_FileName:    TFileName;
            m_Enabled:     Boolean;
            m_WrapMode:    EQRTextureWrapMode;
            m_pCustomData: Pointer;

        public
            { Construction/Destruction }
            constructor Create();  virtual;
            destructor  Destroy(); override;

            { Properties }
            property Index:      NativeUInt         read m_Index       write m_Index;
            property Name:       UnicodeString      read m_Name        write m_Name;
            property Dir:        UnicodeString      read m_Dir         write m_Dir;
            property FileName:   TFileName          read m_FileName    write m_FileName;
            property Enabled:    Boolean            read m_Enabled     write m_Enabled;
            property WrapMode:   EQRTextureWrapMode read m_WrapMode    write m_WrapMode    default EQR_WM_REPEAT;
            property CustomData: Pointer            read m_pCustomData write m_pCustomData default nil;
    end;

    TQRTextures = array of TQRTexture;
    PQRTextures = ^TQRTextures;

    {**
    * Cull mode enumeration
    *}
    EQRCullMode =
    (
        EQR_CM_None = 0,
        EQR_CM_Front,
        EQR_CM_Back,
        EQR_CM_Both
    );

    {**
    * Depth function enumeration
    *}
    EQRDepthFunc =
    (
        EQR_DF_Never = 0,
        EQR_DF_Less,
        EQR_DF_Equal,
        EQR_DF_LessEqual,
        EQR_DF_Greater,
        EQR_DF_NotEqual,
        EQR_DF_GreaterEqual,
        EQR_DF_Always
    );

    {**
    * Depth test info
    *}
    TQRDepthTest = record
        m_Enabled:          Boolean;
        m_DepthMaskEnabled: Boolean;
        m_DepthFunction:    EQRDepthFunc;
        m_DepthRangeMin:    Single;
        m_DepthRangeMax:    Single;
    end;

    PQRDepthTest = ^TQRDepthTest;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVertex
//--------------------------------------------------------------------------------------------------
class function TQRVertex.GetDefault(): TQRVertex;
begin
    Result := Default(TQRVertex);
    Result.Clear;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVertex.Clear;
begin
    m_Name      := '';
    m_Stride    := 0;
    m_Type      := EQR_VT_Unknown;
    m_CoordType := EQR_VC_XYZ;
    m_Format    := [];

    SetLength(m_Buffer, 0);
end;
//--------------------------------------------------------------------------------------------------
function TQRVertex.Clone(): TQRVertex;
begin
    Result.Clear;

    // clone vertex
    Result.m_Name      := m_Name;
    Result.m_Stride    := m_Stride;
    Result.m_Type      := m_Type;
    Result.m_Format    := m_Format;
    Result.m_CoordType := m_CoordType;
end;
//--------------------------------------------------------------------------------------------------
function TQRVertex.CalculateStride(): NativeUInt;
var
    stride: NativeUInt;
begin
    // search for coordinate type
    case m_CoordType of
        EQR_VC_XY:  stride := 2;
        EQR_VC_XYZ: stride := 3;
    else
        raise Exception.Create('Unknown coordinate type');
    end;

    // do include normals?
    if (EQR_VF_Normals in m_Format) then
        Inc(stride, 3);

    // do include texture coordinates?
    if (EQR_VF_TexCoords in m_Format) then
        Inc(stride, 2);

    // do include vertex color?
    if (EQR_VF_Colors in m_Format) then
        Inc(stride, 4);

    Result := stride;
end;
//--------------------------------------------------------------------------------------------------
function TQRVertex.CompareFormat(const other: TQRVertex): Boolean;
begin
    Result := ((m_Stride    = other.m_Stride) and
               (m_Type      = other.m_Type)   and
               (m_Format    = other.m_Format) and
               (m_CoordType = other.m_CoordType));
end;
//--------------------------------------------------------------------------------------------------
// TQRShader
//--------------------------------------------------------------------------------------------------
constructor TQRShader.Create();
begin
    inherited Create();
    PopulateAttributeDict();
end;
//--------------------------------------------------------------------------------------------------
destructor TQRShader.Destroy();
begin
    // clear memory
    m_pAttributeDictionary.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRShader.PopulateAttributeDict();
begin
    m_pAttributeDictionary := TStringList.Create;
    m_pAttributeDictionary.Insert(NativeInt(EQR_SA_Position),            'qr_vPosition');
    m_pAttributeDictionary.Insert(NativeInt(EQR_SA_Normal),              'qr_vNormal');
    m_pAttributeDictionary.Insert(NativeInt(EQR_SA_Texture),             'qr_vTexCoord');
    m_pAttributeDictionary.Insert(NativeInt(EQR_SA_Color),               'qr_vColor');
    m_pAttributeDictionary.Insert(NativeInt(EQR_SA_PerspectiveMatrix),   'qr_uPerspective');
    m_pAttributeDictionary.Insert(NativeInt(EQR_SA_ProjectionMatrix),    'qr_uProjection');
    m_pAttributeDictionary.Insert(NativeInt(EQR_SA_ViewMatrix),          'qr_uViewMatrix');
    m_pAttributeDictionary.Insert(NativeInt(EQR_SA_CameraMatrix),        'qr_uCamera');
    m_pAttributeDictionary.Insert(NativeInt(EQR_SA_ModelMatrix),         'qr_uModel');
    m_pAttributeDictionary.Insert(NativeInt(EQR_SA_Interpolation),       'qr_fInterpolation');
    m_pAttributeDictionary.Insert(NativeInt(EQR_SA_InterpolationPos),    'qr_viPosition');
    m_pAttributeDictionary.Insert(NativeInt(EQR_SA_InterpolationNormal), 'qr_viNormal');
    m_pAttributeDictionary.Insert(NativeInt(EQR_SA_ColorMap),            'qr_sColorMap');
end;
//--------------------------------------------------------------------------------------------------
function TQRShader.GetAttributeName(attribute: EQRShaderAttribute): UnicodeString;
begin
    // no dictionary defined?
    if (not Assigned(m_pAttributeDictionary)) then
    begin
        Result := '';
        Exit;
    end;

    Result := m_pAttributeDictionary.Strings[NativeInt(attribute)];
end;
//--------------------------------------------------------------------------------------------------
procedure TQRShader.SetAttributeName(attribute: EQRShaderAttribute; const name: UnicodeString);
begin
    // is compiling on XE2 or earlier?
    {$IF CompilerVersion < 24}
        // is name empty?
        if (Length(name) = 0) then
    {$ELSE}
        // is name empty?
        if (name.IsEmpty) then
    {$IFEND}
            Exit;

    // no dictionary defined?
    if (not Assigned(m_pAttributeDictionary)) then
        Exit;

    // change attribute name
    m_pAttributeDictionary.Strings[NativeInt(attribute)] := name;
end;
//--------------------------------------------------------------------------------------------------
// TQRRotation
//--------------------------------------------------------------------------------------------------
constructor TQRRotation.Create();
begin
    inherited Create;

    m_Angle := 0.0;
    m_Axis  := TQRVector3D.Create(0.0, 0.0, 0.0);
end;
//--------------------------------------------------------------------------------------------------
constructor TQRRotation.Create(const angle: Single; const axis: TQRVector3D);
begin
    inherited Create;

    m_Angle := angle;
    m_Axis  := axis;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRRotation.Destroy();
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRRotation.GetAngle(): Single;
begin
    Result := m_Angle;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRRotation.SetAngle(angle: Single);
begin
    m_Angle := angle;
end;
//--------------------------------------------------------------------------------------------------
function TQRRotation.GetAxis(): PQRVector3D;
begin
    Result := @m_Axis;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRRotation.SetAxis(pAxis: PQRVector3D);
begin
    m_Axis := pAxis^;
end;
//--------------------------------------------------------------------------------------------------
// TQRTexture
//--------------------------------------------------------------------------------------------------
constructor TQRTexture.Create();
begin
    inherited Create;

    m_Index       := 0;
    m_Enabled     := True;
    m_WrapMode    := EQR_WM_REPEAT;
    m_pCustomData := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRTexture.Destroy();
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------

end.
