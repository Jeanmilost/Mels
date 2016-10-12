// *************************************************************************************************
// * ==> UTQR3D -----------------------------------------------------------------------------------*
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
 @abstract(@name provides the basic classes to work with 3D.)
 @image(Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQR3D;

interface

uses System.Classes,
     System.SysUtils,
     UTQRGeometry;

type
    {$REGION 'Documentation'}
    {**
     Vertex formats enumeration
    }
    {$ENDREGION}
    EQRVertexFormats =
    (
        EQR_VF_Normals,
        EQR_VF_TexCoords,
        EQR_VF_Colors
    );

    {$REGION 'Documentation'}
    {**
     Vertex formats set
    }
    {$ENDREGION}
    TQRVertexFormat = set of EQRVertexFormats;

    {$REGION 'Documentation'}
    {**
     Vertex buffer type enumeration
    }
    {$ENDREGION}
    EQRVertexType =
    (
        EQR_VT_Unknown = 0,
        EQR_VT_Triangles,
        EQR_VT_TriangleStrip,
        EQR_VT_TriangleFan,
        EQR_VT_Quads,
        EQR_VT_QuadStrip
    );

    {$REGION 'Documentation'}
    {**
     Vertex coordinate type
    }
    {$ENDREGION}
    EQRVertexCoordType =
    (
        EQR_VC_Unknown = 0,
        EQR_VC_XY,
        EQR_VC_XYZ
    );

    TQRVertexBuffer = array of Single;

    {$REGION 'Documentation'}
    {**
     Vertex descriptor, contains global enumeration and types
    }
    {$ENDREGION}
    TQRVertex = record
        public
            {$REGION 'Documentation'}
            {**
             Optional name, used e.g. to link the vertex with a texture
            }
            {$ENDREGION}
            m_Name: UnicodeString;

            {$REGION 'Documentation'}
            {**
             Vertex stride (i.e. length between each vertex) in bytes
            }
            {$ENDREGION}
            m_Stride: NativeUInt;

            {$REGION 'Documentation'}
            {**
             Vertex type (i.e. how vertex is organized: triangle list, triangle fan, ...)
            }
            {$ENDREGION}
            m_Type: EQRVertexType;

            {$REGION 'Documentation'}
            {**
             Vertex format (i.e. what data vertex contains: position, normal, texture, ...)
            }
            {$ENDREGION}
            m_Format: TQRVertexFormat;

            {$REGION 'Documentation'}
            {**
             Vertex coordinate type (i.e. 2D coordinates, 3D coordinates, ...)
            }
            {$ENDREGION}
            m_CoordType: EQRVertexCoordType;

            {$REGION 'Documentation'}
            {**
             Vertex buffer
            }
            {$ENDREGION}
            m_Buffer: TQRVertexBuffer;

        public
            {$REGION 'Documentation'}
            {**
             Gets record initialized with default values
             @return(Record initialized with default values)
            }
            {$ENDREGION}
            class function GetDefault: TQRVertex; inline; static;

            {$REGION 'Documentation'}
            {**
             Clears record
            }
            {$ENDREGION}
            procedure Clear; inline;

            {$REGION 'Documentation'}
            {**
             Clones vertex in such manner that vertex info are copied, but not vertex buffer
             @return(Cloned vertex)
             @br @bold(NOTE) Cloned vertex should be deleted when useless
            }
            {$ENDREGION}
            function Clone: TQRVertex; inline;

            {$REGION 'Documentation'}
            {**
             Calculates vertex stride
             @return(Vertex stride)
            }
            {$ENDREGION}
            function CalculateStride: NativeUInt;

            {$REGION 'Documentation'}
            {**
             Compares vertex and determine if their format are equivalent
             @param(other Other vertex to compare with)
             @return(@true if both vertex header are equivalent, otherwise @false)
            }
            {$ENDREGION}
            function CompareFormat(const other: TQRVertex): Boolean; inline;
    end;

    PQRVertex = ^TQRVertex;

    {$REGION 'Documentation'}
    {**
     Mesh, it's a set of vertex buffers representing a model
    }
    {$ENDREGION}
    TQRMesh = array of TQRVertex;
    PQRMesh = ^TQRMesh;

    {$REGION 'Documentation'}
    {**
     Shader type enumeration
    }
    {$ENDREGION}
    EQRShaderType =
    (
        EQR_ST_Vertex,
        EQR_ST_Fragment
    );

    {$REGION 'Documentation'}
    {**
     Shader attributes
     @value(EQR_SA_Position Used to connect the model vertex buffer to the shader program input)
     @value(EQR_SA_Normal Used to connect the model normal buffer to the shader program input)
     @value(EQR_SA_Texture Used to connect the model texture buffer to the shader program input)
     @value(EQR_SA_Color Used to connect the model color buffer to the shader program input)
     @value(EQR_SA_PerspectiveMatrix Used to connect the perpective matrix to the shader program input)
     @value(EQR_SA_ProjectionMatrix Used to connect the projection matrix to the shader program input)
     @value(EQR_SA_ViewMatrix Used to connect the view matrix to the shader program input)
     @value(EQR_SA_CameraMatrix Used to connect the camera matrix to the shader program input)
     @value(EQR_SA_ModelMatrix Used to connect the model matrix to the shader program input)
     @value(EQR_SA_Interpolation Used to connect the interpolation factor to the shader program input)
     @value(EQR_SA_InterpolationPos Used to connect the model vertex buffer to interpolate with the
                                    main vertex buffer to the shader program input)
     @value(EQR_SA_InterpolationNormal Used to connect the model normal buffer to interpolate with
                                       the main normal buffer to the shader program input)
     @value(EQR_SA_ColorMap Used to connect the texture data to the shader program input)
     @br @bold(NOTE) In the shader, EQR_SA_ProjectionMatrix should point to the same data as
                     EQR_SA_PerspectiveMatrix. The both values exist for convenience only and should
                     not be used together in the same shader program
     @br @bold(NOTE) In the shader, EQR_SA_CameraMatrix should point to the same data as
                     EQR_SA_ViewMatrix. The both values exist for convenience only and should
                     not be used together in the same shader program
    }
    {$ENDREGION}
    EQRShaderAttribute =
    (
        EQR_SA_Position,
        EQR_SA_Normal,
        EQR_SA_Texture,
        EQR_SA_Color,
        EQR_SA_PerspectiveMatrix,
        EQR_SA_ProjectionMatrix,
        EQR_SA_ViewMatrix,
        EQR_SA_CameraMatrix,
        EQR_SA_ModelMatrix,
        EQR_SA_Interpolation,
        EQR_SA_InterpolationPos,
        EQR_SA_InterpolationNormal,
        EQR_SA_ColorMap
    );

    TQRAttributeDictionary = TStringList;

    {$REGION 'Documentation'}
    {**
     Basic shader language class
    }
    {$ENDREGION}
    TQRShader = class
        private
            m_pAttributeDictionary: TQRAttributeDictionary;

        protected
            {$REGION 'Documentation'}
            {**
             Populates default attribute dictionary
            }
            {$ENDREGION}
            procedure PopulateAttributeDict; virtual;

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
             Gets attribute name
             @param(attribute Attribute to get name)
             @return(Attribute name)
            }
            {$ENDREGION}
            function GetAttributeName(attribute: EQRShaderAttribute): UnicodeString; virtual;

            {$REGION 'Documentation'}
            {**
             Sets attribute name
             @param(attribute Attribute to set name)
             @param(name New attribute name)
            }
            {$ENDREGION}
            procedure SetAttributeName(attribute: EQRShaderAttribute;
                                      const name: UnicodeString); virtual;

            {$REGION 'Documentation'}
            {**
             Gets shader program identifier
             @return(Shader program identifier)
            }
            {$ENDREGION}
            function GetProgramID: NativeUInt; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Attaches shader to program from stream
             @param(pStream Stream containing shader)
             @param(type Shader type)
             @return(Compiled shader identifier)
            }
            {$ENDREGION}
            function AttachFile(pStream: TStream;
                             shaderType: EQRShaderType): NativeUInt; overload; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Attaches shader to program from file
             @param(fileName Shader file)
             @param(type Shader type)
             @return(Compiled shader identifier)
            }
            {$ENDREGION}
            function AttachFile(const fileName: TFileName;
                                    shaderType: EQRShaderType): NativeUInt; overload; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Attaches shader to program
             @param(source Shader source code)
             @param(type Shader type)
             @return(Compiled shader identifier)
            }
            {$ENDREGION}
            function Attach(const source: UnicodeString;
                              shaderType: EQRShaderType): NativeUInt; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Links all attached shader and keep program ready to run
             @param(use If @true, program will be used immediately (in case link succeeded))
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Link(use: Boolean): Boolean; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Uses the program
             @param(use If @true, program will be used, unused otherwise)
            }
            {$ENDREGION}
            procedure Use(use: Boolean); virtual; abstract;
    end;

    {$REGION 'Documentation'}
    {**
     Rotation info, describe a rotation to apply e.g. to a model
    }
    {$ENDREGION}
    TQRRotation = class
        private
            m_Angle: Single;
            m_Axis:  TQRVector3D;

        protected
            {$REGION 'Documentation'}
            {**
             Gets angle
             @return(Angle in radians)
            }
            {$ENDREGION}
            function GetAngle: Single; virtual;

            {$REGION 'Documentation'}
            {**
             Sets angle
             @param(angle Angle in radians)
            }
            {$ENDREGION}
            procedure SetAngle(angle: Single); virtual;

            {$REGION 'Documentation'}
            {**
             Gets rotation axis
             @return(Rotation axis)
            }
            {$ENDREGION}
            function GetAxis: PQRVector3D; virtual;

            {$REGION 'Documentation'}
            {**
             Sets axis
             @param(pAxis Rotation axis)
            }
            {$ENDREGION}
            procedure SetAxis(pAxis: PQRVector3D); virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Constructor
             @param(angle Rotation angle in radians)
             @param(axis Rotation axis, e.g. [0.0, 1.0, 0.0] for a rotation on the y axis)
            }
            {$ENDREGION}
            constructor Create(const angle: Single; const axis: TQRVector3D); overload; virtual;

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
             Gets or sets the angle in radians
            }
            {$ENDREGION}
            property Angle: Single read GetAngle write SetAngle;

            {$REGION 'Documentation'}
            {**
             Gets or sets the rotation axis
            }
            {$ENDREGION}
            property Axis: PQRVector3D read GetAxis  write SetAxis;
    end;

    {$REGION 'Documentation'}
    {**
     Model texture wrap mode enumeration
    }
    {$ENDREGION}
    EQRTextureWrapMode =
    (
        EQR_WM_CLAMP_TO_EDGE,
        EQR_WM_CLAMP_TO_BORDER,
        EQR_WM_MIRRORED_REPEAT,
        EQR_WM_REPEAT,
        EQR_WM_MIRROR_CLAMP_TO_EDGE
    );

    {$REGION 'Documentation'}
    {**
     Texture coordinate
    }
    {$ENDREGION}
    TQRTexCoord = record
        private
            m_U: Single;
            m_V: Single;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the texture U coordinate
            }
            {$ENDREGION}
            property U: Single read m_U write m_U;

            {$REGION 'Documentation'}
            {**
             Gets or sets the texture V coordinate
            }
            {$ENDREGION}
            property V: Single read m_V write m_V;
    end;

    PQRTexCoord = ^TQRTexCoord;

    {$REGION 'Documentation'}
    {**
     Texture info, contains all info about a texture item
    }
    {$ENDREGION}
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
            destructor  Destroy; override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the texture index
            }
            {$ENDREGION}
            property Index: NativeUInt read m_Index write m_Index;

            {$REGION 'Documentation'}
            {**
             Gets or sets the texture name
            }
            {$ENDREGION}
            property Name: UnicodeString read m_Name write m_Name;

            {$REGION 'Documentation'}
            {**
             Gets or sets the directory name in which the texture is contained
            }
            {$ENDREGION}
            property Dir: UnicodeString read m_Dir write m_Dir;

            {$REGION 'Documentation'}
            {**
             Gets or sets the texture file name
            }
            {$ENDREGION}
            property FileName: TFileName read m_FileName write m_FileName;

            {$REGION 'Documentation'}
            {**
             Gets or sets if the texture is enabled or disabled
            }
            {$ENDREGION}
            property Enabled: Boolean read m_Enabled write m_Enabled;

            {$REGION 'Documentation'}
            {**
             Gets or sets the texture wrap mode, the default value is EQR_WM_REPEAT
            }
            {$ENDREGION}
            property WrapMode: EQRTextureWrapMode read m_WrapMode write m_WrapMode default EQR_WM_REPEAT;

            {$REGION 'Documentation'}
            {**
             Gets or sets the texture custom data, the default value is @nil
            }
            {$ENDREGION}
            property CustomData: Pointer read m_pCustomData write m_pCustomData default nil;
    end;

    TQRTextures = array of TQRTexture;
    PQRTextures = ^TQRTextures;

    {$REGION 'Documentation'}
    {**
     Cull mode enumeration
    }
    {$ENDREGION}
    EQRCullMode =
    (
        EQR_CM_None = 0,
        EQR_CM_Front,
        EQR_CM_Back,
        EQR_CM_Both
    );

    {$REGION 'Documentation'}
    {**
     Depth function enumeration
    }
    {$ENDREGION}
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

    {$REGION 'Documentation'}
    {**
     Depth test info
    }
    {$ENDREGION}
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
class function TQRVertex.GetDefault: TQRVertex;
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
function TQRVertex.Clone: TQRVertex;
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
function TQRVertex.CalculateStride: NativeUInt;
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
constructor TQRShader.Create;
begin
    inherited Create;
    PopulateAttributeDict;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRShader.Destroy;
begin
    // clear memory
    m_pAttributeDictionary.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRShader.PopulateAttributeDict;
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
    // is name empty?
    if (Length(name) = 0) then
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
constructor TQRRotation.Create;
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
destructor TQRRotation.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRRotation.GetAngle: Single;
begin
    Result := m_Angle;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRRotation.SetAngle(angle: Single);
begin
    m_Angle := angle;
end;
//--------------------------------------------------------------------------------------------------
function TQRRotation.GetAxis: PQRVector3D;
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
constructor TQRTexture.Create;
begin
    inherited Create;

    m_Index       := 0;
    m_Enabled     := True;
    m_WrapMode    := EQR_WM_REPEAT;
    m_pCustomData := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRTexture.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------

end.
