// *************************************************************************************************
// * ==> UTQRVCLMD2ModelComponentGL ---------------------------------------------------------------*
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
 @abstract(@name provides a MD2 model component using OpenGL to draw it.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRVCLMD2ModelComponentGL;

interface
    // do not include Winapi.OpenGLExt in hpp, because it may generate conflicts in C++ code
    (*$NOINCLUDE Winapi.OpenGLext *)

uses System.Classes,
     System.SysUtils,
     UTQRHelpers,
     UTQRVCLHelpers,
     UTQRLogging,
     UTQRGeometry,
     UTQR3D,
     UTQRCollision,
     UTQRModel,
     UTQRMD2,
     UTQRModelGroup,
     UTQRMD2ModelGroup,
     UTQRVCLModelComponentGL,
     UTQRVCLModelComponentPropertiesGL,
     Vcl.Graphics,
     Vcl.Controls,
     Winapi.Messages,
     Winapi.Windows,
     Winapi.OpenGL,
     Winapi.OpenGLext;

type
    {$REGION 'Documentation'}
    {**
     MD2 model component
    }
    {$ENDREGION}
    TQRVCLMD2ModelGL = class(TQRVCLFramedModelComponentGL)
        private
            m_pMD2:                TQRMD2Group;
            m_pModel:              TQRVCLModelComponentPropertyGL;
            m_pPreCalculatedLight: TQRVCLPreCalculatedLightComponentPropertyGL;
            m_pPackage:            TMemoryStream;
            m_pVertexShader:       TMemoryStream;
            m_pFragmentShader:     TMemoryStream;
            m_VertexName:          TFileName;
            m_FragmentName:        TFileName;
            m_PackageName:         TFileName;
            m_Gesture:             EQRMD2AnimationGesture;
            m_ModelOptions:        TQRModelOptions;
            m_FramedModelOptions:  TQRFramedModelOptions;
            m_hSceneDC:            THandle;

        protected
            {$REGION 'Documentation'}
            {**
             Sets model package file name
             @param(fileName File name)
            }
            {$ENDREGION}
            procedure SetPackageName(fileName: TFileName); virtual;

            {$REGION 'Documentation'}
            {**
             Sets model vertex shader file name
             @param(fileName File name)
            }
            {$ENDREGION}
            procedure SetVertexName(fileName: TFileName); virtual;

            {$REGION 'Documentation'}
            {**
             Sets model fragment shader file name
             @param(fileName File name)
            }
            {$ENDREGION}
            procedure SetFragmentName(fileName: TFileName); virtual;

            {$REGION 'Documentation'}
            {**
             Sets model gesture
             @param(gesture Gesture)
            }
            {$ENDREGION}
            procedure SetGesture(gesture: EQRMD2AnimationGesture); virtual;

            {$REGION 'Documentation'}
            {**
             Sets model options
             @param(options Options)
            }
            {$ENDREGION}
            procedure SetModelOptions(options: TQRModelOptions); virtual;

            {$REGION 'Documentation'}
            {**
             Sets framed model options
             @param(options Options)
            }
            {$ENDREGION}
            procedure SetFramedModelOptions(options: TQRFramedModelOptions); virtual;

            {$REGION 'Documentation'}
            {**
             Declares properties that will deal with DFM files
             @param(pFiler DFM file manager)
            }
            {$ENDREGION}
            procedure DefineProperties(pFiler: TFiler); override;

            {$REGION 'Documentation'}
            {**
             Reads package content from DFM file
             @param(pStream Stream containing DFM data)
            }
            {$ENDREGION}
            procedure ReadPackage(pStream: TStream); virtual;

            {$REGION 'Documentation'}
            {**
             Writes package content to DFM file
             @param(pStream DFM stream in which package should be written)
            }
            {$ENDREGION}
            procedure WritePackage(pStream: TStream); virtual;

            {$REGION 'Documentation'}
            {**
             Reads vertex shader content from DFM file
             @param(pStream Stream containing DFM data)
            }
            {$ENDREGION}
            procedure ReadVertexShader(pStream: TStream); virtual;

            {$REGION 'Documentation'}
            {**
             Writes vertex shader content to DFM file
             @param(pStream DFM stream in which vertex shader should be written)
            }
            {$ENDREGION}
            procedure WriteVertexShader(pStream: TStream); virtual;

            {$REGION 'Documentation'}
            {**
             Reads fragment shader content from DFM file
             @param(pStream Stream containing DFM data)
            }
            {$ENDREGION}
            procedure ReadFragmentShader(pStream: TStream); virtual;

            {$REGION 'Documentation'}
            {**
             Writes fragment shader content to DFM file
             @param(pStream DFM stream in which fragment shader should be written)
            }
            {$ENDREGION}
            procedure WriteFragmentShader(pStream: TStream); virtual;

            {$REGION 'Documentation'}
            {**
             Called after control was fully loaded from DFM stream
            }
            {$ENDREGION}
            procedure Loaded; override;

            {$REGION 'Documentation'}
            {**
             Creates the component Windows handle
             @param(params Windows parameters used to create handle)
            }
            {$ENDREGION}
            procedure CreateWindowHandle(const params: TCreateParams); override;

            {$REGION 'Documentation'}
            {**
             Deletes the component Windows handle
            }
            {$ENDREGION}
            procedure DestroyWindowHandle; override;

            {$REGION 'Documentation'}
            {**
             Creates a viewport for the component
             @param(width Viewport width)
             @param(height Viewport height)
            }
            {$ENDREGION}
            procedure CreateViewport(width, height: NativeUInt); override;

            {$REGION 'Documentation'}
            {**
             Loads the model
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadModel: Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Prepares shader to be used while scene is drawn
             @param(textures Model textures)
            }
            {$ENDREGION}
            function PrepareShaderToDrawModel(const textures: TQRTextures): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Called after model was completely loaded
             @param(pGroup Group that finished to load the model)
            }
            {$ENDREGION}
            procedure OnAfterLoadModelEvent(const pGroup: TQRModelGroup); virtual;

            {$REGION 'Documentation'}
            {**
             Called when mesh texture should be loaded
             @param(pModel Model for which texture should be loaded)
             @param(pBitmap Whenever possible, the bitmap containing the texture, @nil if not
                            available)
             @param(pTexture Texture info)
             @param(loadNext @bold([out]) If @true, event will be called again with a new item to
                                          load next texture)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function OnLoadMeshTexture(const pGroup: TQRModelGroup;
                                       const pModel: TQRModel;
                                            pBitmap: Vcl.Graphics.TBitmap;
                                           pTexture: TQRTexture;
                                       out loadNext: Boolean): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Called when the scene content should be drawn
             @param(hDC Internal control device context that OpenGL should use to draw the scene)
            }
            {$ENDREGION}
            procedure OnDrawSceneContent(hDC: THandle); override;

            {$REGION 'Documentation'}
            {**
             Called when framed model item should be drawn
             @param(pGroup Group at which model belongs)
             @param(pModel Model to draw)
             @param(textures Textures belonging to model, in the order where they should be combined)
             @param(matrix Model matrix)
             @param(index Model mesh index)
             @param(nextIndex Model mesh index to interpolate with)
             @param(interpolationFactor Interpolation factor)
            }
            {$ENDREGION}
            procedure OnCustomDrawModelItem(const pGroup: TQRModelGroup;
                                                  pModel: TQRModel;
                                          const textures: TQRTextures;
                                            const matrix: TQRMatrix4x4;
                                        index, nextIndex: NativeInt;
                               const interpolationFactor: Double); virtual;

            {$REGION 'Documentation'}
            {**
             Called when framed model item should be drawn
             @param(pGroup Group at which model belongs)
             @param(pModel Model to draw)
             @param(textures Textures belonging to model, in the order where they should be combined)
             @param(matrix Model matrix)
             @param(index Model mesh index)
             @param(nextIndex Model mesh index to interpolate with)
             @param(interpolationFactor Interpolation factor)
             @param(pMesh Mesh to draw, can be @nil)
             @param(pNextMesh Next mesh to interpolate with, can be @nil)
             @param(pAABBTree Aligned-axis bounding box tree matching with mesh, can be @nil)
             @param(pNextAABBTree Aligned-axis bounding box tree matching with next mesh, can be @nil)
            }
            {$ENDREGION}
            procedure OnDrawModelItem(const pGroup: TQRModelGroup;
                                      const pModel: TQRModel;
                                    const textures: TQRTextures;
                                      const matrix: TQRMatrix4x4;
                                  index, nextIndex: NativeInt;
                         const interpolationFactor: Double;
                            const pMesh, pNextMesh: PQRMesh;
                    const pAABBTree, pNextAABBTree: TQRAABBTree); virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
             @param(pOwner Component owner)
            }
            {$ENDREGION}
            constructor Create(pOwner: TComponent); override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Copies the property attributes from another property
             @param(pSource Source property to copy from)
            }
            {$ENDREGION}
            procedure Assign(pSource: TPersistent); override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets the MD2
            }
            {$ENDREGION}
            property MD2: TQRMD2Group read m_pMD2;

        // Properties
        published
            {$REGION 'Documentation'}
            {**
             Gets or sets the model properties set
            }
            {$ENDREGION}
            property Model: TQRVCLModelComponentPropertyGL read m_pModel write m_pModel;

            {$REGION 'Documentation'}
            {**
             Gets or sets the pre-calculated properties set
            }
            {$ENDREGION}
            property PreCalculatedLight: TQRVCLPreCalculatedLightComponentPropertyGL read m_pPreCalculatedLight write m_pPreCalculatedLight;

            {$REGION 'Documentation'}
            {**
             Gets or sets the MD2 package name to load
            }
            {$ENDREGION}
            property PackageName: TFileName read m_PackageName write SetPackageName;

            {$REGION 'Documentation'}
            {**
             Gets or sets the vertex shader file name to load
            }
            {$ENDREGION}
            property VertexName: TFileName read m_VertexName write SetVertexName;

            {$REGION 'Documentation'}
            {**
             Gets or sets the fragment shader file name to load
            }
            {$ENDREGION}
            property FragmentName: TFileName read m_FragmentName write SetFragmentName;

            {$REGION 'Documentation'}
            {**
             Gets or sets the model gesture to execute, default is EQR_AG_MD2_Stand
            }
            {$ENDREGION}
            property Gesture: EQRMD2AnimationGesture read m_Gesture write SetGesture default EQR_AG_MD2_Stand;

            {$REGION 'Documentation'}
            {**
             Gets or sets the model options, default are EQR_MO_Dynamic_Frames and EQR_MO_No_Collision
            }
            {$ENDREGION}
            property ModelOptions: TQRModelOptions read m_ModelOptions write SetModelOptions default [EQR_MO_Dynamic_Frames, EQR_MO_No_Collision];

            {$REGION 'Documentation'}
            {**
             Gets or sets the framed model options, none are enabled by default
            }
            {$ENDREGION}
            property FramedModelOptions: TQRFramedModelOptions read m_FramedModelOptions write SetFramedModelOptions;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVCLMD2ModelGL
//--------------------------------------------------------------------------------------------------
constructor TQRVCLMD2ModelGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // override basic data, to enable double-buffering on OpenGL rendering (means also that embedded
    // GDI rendering isn't supported for this component)
    SupportsGDI := False;

    // initialize variables
    m_pMD2                := TQRMD2Group.Create;
    m_pModel              := TQRVCLModelComponentPropertyGL.Create(Self, OnReceivePropNotification);
    m_pPreCalculatedLight := TQRVCLPreCalculatedLightComponentPropertyGL.Create(Self, OnReceivePropNotification);
    m_pPackage            := TMemoryStream.Create;
    m_pVertexShader       := TMemoryStream.Create;
    m_pFragmentShader     := TMemoryStream.Create;
    m_Gesture             := EQR_AG_MD2_Stand;
    m_ModelOptions        := [EQR_MO_Dynamic_Frames, EQR_MO_No_Collision];
    m_FramedModelOptions  := [];
    m_hSceneDC            := 0;

    // configure model
    m_pMD2.OnAfterLoadModelEvent := OnAfterLoadModelEvent;
    m_pMD2.OnLoadMeshTexture     := OnLoadMeshTexture;
    m_pMD2.OnCustomDrawItem      := OnCustomDrawModelItem;
    m_pMD2.OnDrawItem            := OnDrawModelItem;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLMD2ModelGL.Destroy;
begin
    // clear memory
    m_pFragmentShader.Free;
    m_pVertexShader.Free;
    m_pPackage.Free;
    m_pPreCalculatedLight.Free;
    m_pModel.Free;
    m_pMD2.Free;

    // set explicitly the MD2 model to nil after deleted it, because in some situations it can be
    // accessed later in destruction, e.g. when handle is destroyed after a control was deleted from
    // designer
    m_pMD2 := nil;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.SetPackageName(fileName: TFileName);
var
    fileStream: TFileStream;
begin
    // no changes?
    if (fileName = m_PackageName) then
        Exit;

    // if component is currently loading, just update the property name, the model will be loaded
    // during the serialization
    if (csLoading in ComponentState) then
    begin
        m_PackageName := fileName;
        Exit;
    end;

    // previous package was loaded?
    if (m_pPackage.Size > 0) then
        // clear it
        m_pPackage.Clear;

    // file name isn't empty, file exists and is a MD2 package file?
    if ((Length(fileName) = 0)     or
        (not FileExists(fileName)) or
        (LowerCase(ExtractFileExt(fileName)) <> '.pk2'))
    then
    begin
        // clear previously loaded model
        m_PackageName := '';
        m_pMD2.Clear;
        Exit;
    end;

    // update name and open file stream
    m_PackageName := fileName;
    fileStream    := TFileStream.Create(m_PackageName, fmOpenRead);

    try
        // copy package file content to memory
        m_pPackage.CopyFrom(fileStream, fileStream.Size);
    finally
        fileStream.Free;
    end;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.SetVertexName(fileName: TFileName);
var
    fileStream: TFileStream;
begin
    // no changes?
    if (fileName = m_VertexName) then
        Exit;

    // if component is currently loading, just update the property name, the model will be loaded
    // during the serialization
    if (csLoading in ComponentState) then
    begin
        m_VertexName := fileName;
        Exit;
    end;

    // previous vertex shader was loaded?
    if (m_pVertexShader.Size > 0) then
        // clear it
        m_pVertexShader.Clear;

    // file name isn't empty, file exists and is a vertex shader file?
    if ((Length(fileName) = 0)     or
        (not FileExists(fileName)) or
        (LowerCase(ExtractFileExt(fileName)) <> '.glsl'))
    then
    begin
        m_VertexName := '';
        RecreateWnd;
        Exit;
    end;

    // update name and open file stream
    m_VertexName := fileName;
    fileStream   := TFileStream.Create(m_VertexName, fmOpenRead);

    try
        // copy vertex shader file content to memory
        m_pVertexShader.CopyFrom(fileStream, fileStream.Size);
    finally
        fileStream.Free;
    end;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.SetFragmentName(fileName: TFileName);
var
    fileStream: TFileStream;
begin
    // no changes?
    if (fileName = m_FragmentName) then
        Exit;

    // if component is currently loading, just update the property name, the model will be loaded
    // during the serialization
    if (csLoading in ComponentState) then
    begin
        m_FragmentName := fileName;
        Exit;
    end;

    // previous fragment shader was loaded?
    if (m_pFragmentShader.Size > 0) then
        // clear it
        m_pFragmentShader.Clear;

    // file name isn't empty, file exists and is a fragment shader file?
    if ((Length(fileName) = 0)     or
        (not FileExists(fileName)) or
        (LowerCase(ExtractFileExt(fileName)) <> '.glsl'))
    then
    begin
        m_FragmentName := '';
        RecreateWnd;
        Exit;
    end;

    // update name and open file stream
    m_FragmentName := fileName;
    fileStream     := TFileStream.Create(m_FragmentName, fmOpenRead);

    try
        // copy fragment shader file content to memory
        m_pFragmentShader.CopyFrom(fileStream, fileStream.Size);
    finally
        fileStream.Free;
    end;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.SetGesture(gesture: EQRMD2AnimationGesture);
begin
    // no changes?
    if (m_Gesture = gesture) then
        Exit;

    m_Gesture := gesture;

    // set gesture to run
    m_pMD2.Gesture := NativeUInt(m_Gesture);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.SetModelOptions(options: TQRModelOptions);
begin
    // no changes?
    if (m_ModelOptions = options) then
        Exit;

    m_ModelOptions := options;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.SetFramedModelOptions(options: TQRFramedModelOptions);
begin
    // no changes?
    if (m_FramedModelOptions = options) then
        Exit;

    m_FramedModelOptions := options;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.DefineProperties(pFiler: TFiler);
    function DoWritePackage: Boolean;
    begin
        if Assigned(pFiler.Ancestor) then
            Result := not (pFiler.Ancestor is TQRVCLMD2ModelGL)
        else
            Result := m_pPackage.Size > 0;
    end;

    function DoWriteVertexShader: Boolean;
    begin
        if Assigned(pFiler.Ancestor) then
            Result := not (pFiler.Ancestor is TQRVCLMD2ModelGL)
        else
            Result := m_pVertexShader.Size > 0;
    end;

    function DoWriteFragmentShader: Boolean;
    begin
        if Assigned(pFiler.Ancestor) then
            Result := not (pFiler.Ancestor is TQRVCLMD2ModelGL)
        else
            Result := m_pFragmentShader.Size > 0;
    end;
begin
    inherited DefineProperties(pFiler);

    // register the properties that will load and save a binary data in DFM files
    pFiler.DefineBinaryProperty('Package',        ReadPackage,        WritePackage,        DoWritePackage);
    pFiler.DefineBinaryProperty('VertexShader',   ReadVertexShader,   WriteVertexShader,   DoWriteVertexShader);
    pFiler.DefineBinaryProperty('FragmentShader', ReadFragmentShader, WriteFragmentShader, DoWriteFragmentShader);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.ReadPackage(pStream: TStream);
begin
    // previous package was loaded?
    if (m_pPackage.Size > 0) then
        // clear it
        m_pPackage.Clear;

    // read model package from DFM stream
    m_pPackage.CopyFrom(pStream, pStream.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.WritePackage(pStream: TStream);
begin
    // reset stream position to start
    m_pPackage.Position := 0;

    // write model package to DFM stream
    pStream.CopyFrom(m_pPackage, m_pPackage.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.ReadVertexShader(pStream: TStream);
begin
    // previous vertex shader was loaded?
    if (m_pVertexShader.Size > 0) then
        // clear it
        m_pVertexShader.Clear;

    // read vertex shader from DFM stream
    m_pVertexShader.CopyFrom(pStream, pStream.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.WriteVertexShader(pStream: TStream);
begin
    // reset stream position to start
    m_pVertexShader.Position := 0;

    // write vertex shader to DFM stream
    pStream.CopyFrom(m_pVertexShader, m_pVertexShader.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.ReadFragmentShader(pStream: TStream);
begin
    // previous fragment shader was loaded?
    if (m_pFragmentShader.Size > 0) then
        // clear it
        m_pFragmentShader.Clear;

    // read fragment shader from DFM stream
    m_pFragmentShader.CopyFrom(pStream, pStream.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.WriteFragmentShader(pStream: TStream);
begin
    // reset stream position to start
    m_pFragmentShader.Position := 0;

    // write fragment shader to DFM stream
    pStream.CopyFrom(m_pFragmentShader, m_pFragmentShader.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.Loaded;
begin
    inherited Loaded;

    // this situation may occur after a new component was created on the designer, e.g. after a
    // copy/paste operation. In this case, the model may fail to load, although the OpenGL context
    // is initialized
    if ((csDesigning in ComponentState) and
         HandleAllocated                and
         Assigned(m_pMD2)               and
         m_pMD2.IsEmpty                 and
        (m_pPackage.Size > 0))
    then
    begin
        RecreateWnd;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.CreateWindowHandle(const params: TCreateParams);
begin
    inherited CreateWindowHandle(params);

    // create a viewport
    CreateViewport(ClientWidth, ClientHeight);

    // (re)load model, done here because model is linked to current context. If context is killed,
    // some things in the model, as e.g. his texture, will become invalid and no more be painted
    // correctly
    LoadModel;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.DestroyWindowHandle;
begin
    // as model is linked to the current context, clears it if context is shutting down
    if (Assigned(m_pMD2)) then
        m_pMD2.Clear;

    inherited DestroyWindowHandle;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.CreateViewport(width, height: NativeUInt);
var
    factor:                  NativeInt;
    position, direction, up: TQRVector3D;
    hDC:                     THandle;
begin
    // cannot create a viewport if there is no client surface to render to it
    if ((ClientWidth = 0) or (ClientHeight = 0)) then
        Exit;

    // no render surface?
    if (not Assigned(RenderSurface)) then
        Exit;

    // check if handle was successfully allocated
    if (not HandleAllocated) then
        Exit;

    // get the device context for this control
    hDC := GetDC(WindowHandle);

    // found it?
    if (hDC = 0) then
        Exit;

    try
        // enable render surface context
        if (not RenderSurface.EnableContext(hDC)) then
            Exit;

        // resize render surface
        RenderSurface.Resize(hDC);

        // resize local overlay, if any
        if (Assigned(Overlay)) then
            Overlay.SetSize(ClientWidth, ClientHeight);

        // get antialiasing factor to apply
        factor := GetAntialiasingFactor;

        // create OpenGL viewport to use to draw scene
        Renderer.CreateViewport(ClientWidth * factor, ClientHeight * factor);

        // notify user that scene matrix (i.e. projection and view matrix) are about to be created
        if (Assigned(OnCreateSceneMatrix)) then
            // user defined his own matrix?
            if (OnCreateSceneMatrix(Self,
                                    ProjectionMatrix^,
                                    ViewMatrix^,
                                    hDC,
                                    RenderSurface.GLContext,
                                    Renderer,
                                    Shader)) then
                Exit;

        // create projection matrix
        ProjectionMatrix.Assign(Renderer.GetProjection(45.0,
                                                       ClientWidth  * factor,
                                                       ClientHeight * factor,
                                                       1.0,
                                                       1000.0));

        position  := TQRVector3D.Create(0.0, 0.0, 0.0);
        direction := TQRVector3D.Create(0.0, 0.0, 1.0);
        up        := TQRVector3D.Create(0.0, 1.0, 0.0);

        // create view matrix (will not be modified while execution)
        ViewMatrix.Assign(Renderer.LookAtLH(position, direction, up));

        // do use shader?
        if (not UseShader) then
        begin
            // load projection matrix and initialize it
            glMatrixMode(GL_PROJECTION);
            glLoadIdentity;

            // apply projection matrix
            glLoadMatrix(PGLfloat(ProjectionMatrix.GetPtr));

            // load model view matrix and initialize it
            glMatrixMode(GL_MODELVIEW);
            glLoadIdentity;

            // apply model view matrix
            glLoadMatrix(PGLfloat(ViewMatrix.GetPtr));
        end;
    finally
        ReleaseDC(WindowHandle, hDC);
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLMD2ModelGL.LoadModel: Boolean;
var
    pPackage:  TMemoryStream;
    pMD2Light: TQRMD2Light;
begin
    // do nothing in case component is loading (in this case model will be loaded immediately after)
    // or deleting (in this case model will no more be loaded)
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
    begin
        Result := False;
        Exit;
    end;

    // no model to load?
    if (m_pPackage.Size <= 0) then
    begin
        Result := False;
        Exit;
    end;

    // is OpenGL context created?
    if (RenderSurface.GLContext = 0) then
    begin
        Result := False;
        Exit;
    end;

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // reset streams position to start
        m_pVertexShader.Position   := 0;
        m_pFragmentShader.Position := 0;

        // try to build shader
        if (not BuildShader(m_pVertexShader, m_pFragmentShader)) then
        begin
            Result := False;
            Exit;
        end;
    end;

    SetModelLoaded(False);

    // reset stream position to start
    m_pPackage.Position := 0;

    pPackage := nil;

    try
        pPackage := TMemoryStream.Create;

        // copy package to load inside a local stream, because the MD2 group will take the
        // ownership of the stream pointer
        pPackage.CopyFrom(m_pPackage, m_pPackage.Size);
    except
        on e: Exception do
        begin
            // clear memory
            pPackage.Free;

            Result := False;
            Exit;
        end;
    end;

    // reset stream position to start
    pPackage.Position := 0;

    // apply basic changes to model before loading it
    m_pModel.Apply(m_pMD2);

    // configure pre-calculated light to use, if any
    if (m_pPreCalculatedLight.Enabled) then
    begin
        pMD2Light := TQRMD2Light.Create;
        m_pPreCalculatedLight.Apply(pMD2Light);
    end
    else
        pMD2Light := nil;

    // load model
    if (not m_pMD2.Load(pPackage,
                        m_pModel.Color.NativeColor,
                        pMD2Light,
                        False,
                        m_ModelOptions,
                        m_FramedModelOptions))
    then
    begin
        pMD2Light.Free;
        Result := False;
        Exit;
    end;

    // set gesture to run
    m_pMD2.Gesture := NativeUInt(m_Gesture);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLMD2ModelGL.PrepareShaderToDrawModel(const textures: TQRTextures): Boolean;
var
    uniform: GLint;
begin
    // OpenGL was not initialized correctly?
    if (not IsAllowed) then
    begin
        Result := False;
        Exit;
    end;

    // bind shader program
    Shader.Use(True);

    // get perspective (or projection) matrix slot from shader
    uniform := Renderer.GetUniform(Shader, EQR_SA_PerspectiveMatrix);

    // found it?
    if (uniform = -1) then
    begin
        TQRLogHelper.LogToCompiler('Program uniform not found - perspective');
        Result := False;
        Exit;
    end;

    // connect perspective (or projection) matrix to shader
    glUniformMatrix4fv(uniform, 1, GL_FALSE, PGLfloat(ProjectionMatrix.GetPtr));

    // get view (or camera) matrix slot from shader
    uniform := Renderer.GetUniform(Shader, EQR_SA_CameraMatrix);

    // found it?
    if (uniform = -1) then
    begin
        TQRLogHelper.LogToCompiler('Program uniform not found - camera');
        Result := False;
        Exit;
    end;

    // connect view (or camera) matrix to shader
    glUniformMatrix4fv(uniform, 1, GL_FALSE, PGLfloat(ViewMatrix.GetPtr));

    // unbind shader program
    Shader.Use(False);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.OnAfterLoadModelEvent(const pGroup: TQRModelGroup);
begin
    SetModelLoaded(True);

    // no animation is running or component is in design time?
    if (NoAnimation or (csDesigning in ComponentState)) then
        // invalidate model to repaint it
        Invalidate;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLMD2ModelGL.OnLoadMeshTexture(const pGroup: TQRModelGroup;
                                            const pModel: TQRModel;
                                                 pBitmap: Vcl.Graphics.TBitmap;
                                                pTexture: TQRTexture;
                                            out loadNext: Boolean): Boolean;
var
    pixels:      TQRByteArray;
    pixelFormat: GLenum;
    hDC:         THandle;
begin
    // no model?
    if (not Assigned(pModel)) then
    begin
        Result := False;
        Exit;
    end;

    // no texture?
    if (not Assigned(pTexture)) then
    begin
        Result := False;
        Exit;
    end;

    // no texture bitmap?
    if (not Assigned(pBitmap)) then
    begin
        Result := False;
        Exit;
    end;

    // check if handle was successfully allocated
    if (not HandleAllocated) then
    begin
        Result := False;
        Exit;
    end;

    // get the device context for this control
    hDC := GetDC(WindowHandle);

    // found it?
    if (hDC = 0) then
    begin
        Result := False;
        Exit;
    end;

    try
        // enable OpenGL rendering context
        if (not RenderSurface.EnableContext(hDC)) then
        begin
            Result := False;
            Exit;
        end;

        // select pixel format to use
        if (pBitmap.PixelFormat = pf32bit) then
            pixelFormat := GL_RGBA
        else
            pixelFormat := GL_RGB;

        try
            // convert bitmap to pixel array, and create OpenGL texture from array
            TQRVCLPictureHelper.BytesFromBitmap(pBitmap, pixels, false, false);
            pTexture.Index := Renderer.CreateTexture(pBitmap.Width,
                                                     pBitmap.Height,
                                                     pixelFormat,
                                                     pixels,
                                                     GL_NEAREST,
                                                     GL_NEAREST,
                                                     GL_TEXTURE_2D);
        finally
            SetLength(pixels, 0);
        end;

        // notify user that a texture should be loaded for the model
        if ((not(csDesigning in ComponentState)) and Assigned(OnLoadTexture)) then
            OnLoadTexture(Self,
                          hDC,
                          RenderSurface.GLContext,
                          Renderer,
                          Shader);
    finally
        ReleaseDC(WindowHandle, hDC);
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.OnDrawSceneContent(hDC: THandle);
begin
    // apply basic changes to model before drawing it
    m_pModel.Apply(m_pMD2);

    try
        m_hSceneDC := hDC;

        // draw model
        m_pMD2.Draw(ElapsedTime);
    finally
        m_hSceneDC := 0;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.OnCustomDrawModelItem(const pGroup: TQRModelGroup;
                                                       pModel: TQRModel;
                                               const textures: TQRTextures;
                                                 const matrix: TQRMatrix4x4;
                                             index, nextIndex: NativeInt;
                                    const interpolationFactor: Double);
begin
    if ((csDesigning in ComponentState) or not(Assigned(OnDrawSceneFramedModelItem))) then
        Exit;

    OnDrawSceneFramedModelItem(Self,
                               m_hSceneDC,
                               RenderSurface.GLContext,
                               Renderer,
                               Shader,
                               pGroup,
                               pModel,
                               textures,
                               matrix,
                               index,
                               nextIndex,
                               interpolationFactor,
                               nil,
                               nil,
                               nil,
                               nil);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.OnDrawModelItem(const pGroup: TQRModelGroup;
                                           const pModel: TQRModel;
                                         const textures: TQRTextures;
                                           const matrix: TQRMatrix4x4;
                                       index, nextIndex: NativeInt;
                              const interpolationFactor: Double;
                                 const pMesh, pNextMesh: PQRMesh;
                         const pAABBTree, pNextAABBTree: TQRAABBTree);
var
    mesh: TQRMesh;
begin
    // notify user that model item is about to be drawn on the scene, stop drawing if user already
    // processed it
    if ((not(csDesigning in ComponentState)) and Assigned(OnDrawSceneFramedModelItem)) then
        if (OnDrawSceneFramedModelItem(Self,
                                       m_hSceneDC,
                                       RenderSurface.GLContext,
                                       Renderer,
                                       Shader,
                                       pGroup,
                                       pModel,
                                       textures,
                                       matrix,
                                       index,
                                       nextIndex,
                                       interpolationFactor,
                                       pMesh,
                                       pNextMesh,
                                       pAABBTree,
                                       pNextAABBTree))
        then
            Exit;

    // no model?
    if (not Assigned(pModel)) then
        Exit;

    // no mesh?
    if (not Assigned(pMesh)) then
        Exit;

    // do use shader?
    if ((m_pVertexShader.Position > 0) and (m_pFragmentShader.Position > 0)) then
    begin
        // prepare shader to draw the model
        PrepareShaderToDrawModel(textures);

        // draw mesh
        Renderer.Draw(pMesh^,
                      pNextMesh^,
                      matrix,
                      interpolationFactor,
                      textures,
                      Shader);

        // notify user that collisions may be detected
        if (Assigned(OnDetectCollisions) and not(EQR_MO_No_Collision in m_ModelOptions)) then
            OnDetectCollisions(Self, ProjectionMatrix^, matrix, pAABBTree, Renderer, Shader);

        Exit;
    end;

    // do interpolate frames?
    if (not Assigned(pNextMesh) or (interpolationFactor <= 0.0)) then
    begin
        // draw mesh
        Renderer.Draw(pMesh^, matrix, textures);

        // notify user that collisions may be detected
        if (Assigned(OnDetectCollisions) and not(EQR_MO_No_Collision in m_ModelOptions)) then
            OnDetectCollisions(Self, ProjectionMatrix^, matrix, pAABBTree, Renderer, Shader);

        Exit;
    end
    else
    if (interpolationFactor >= 1.0) then
    begin
        // draw mesh
        Renderer.Draw(pNextMesh^, matrix, textures);

        // notify user that collisions may be detected
        if (Assigned(OnDetectCollisions) and not(EQR_MO_No_Collision in m_ModelOptions)) then
            OnDetectCollisions(Self, ProjectionMatrix^, matrix, pNextAABBTree, Renderer, Shader);

        Exit;
    end;

    // get next frame to draw
    TQRModelHelper.Interpolate(interpolationFactor, pMesh^, pNextMesh^, mesh);

    // draw mesh
    Renderer.Draw(mesh, matrix, textures);

    // notify user that collisions may be detected
    if (Assigned(OnDetectCollisions) and not(EQR_MO_No_Collision in m_ModelOptions)) then
        OnDetectCollisions(Self, ProjectionMatrix^, matrix, pAABBTree, Renderer, Shader);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLMD2ModelGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLMD2ModelGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLMD2ModelGL)) then
    begin
        // reset values to default
        m_PackageName        := '';
        m_VertexName         := '';
        m_FragmentName       := '';
        m_Gesture            := EQR_AG_MD2_Stand;
        m_ModelOptions       := [EQR_MO_Dynamic_Frames, EQR_MO_No_Collision];
        m_FramedModelOptions := [];

        m_pPackage.Clear;
        m_pVertexShader.Clear;
        m_pFragmentShader.Clear;

        m_pModel.Assign(nil);
        m_pPreCalculatedLight.Assign(nil);

        m_pMD2.Gesture := NativeUInt(m_Gesture);
        Exit;
    end;

    // copy content from source
    pSrc                 := pSource as TQRVCLMD2ModelGL;
    m_PackageName        := pSrc.m_PackageName;
    m_VertexName         := pSrc.m_VertexName;
    m_FragmentName       := pSrc.m_FragmentName;
    m_Gesture            := pSrc.m_Gesture;
    m_ModelOptions       := pSrc.m_ModelOptions;
    m_FramedModelOptions := pSrc.m_FramedModelOptions;

    m_pPackage.CopyFrom(pSrc.m_pPackage, pSrc.m_pPackage.Size);
    m_pVertexShader.CopyFrom(pSrc.m_pVertexShader, pSrc.m_pVertexShader.Size);
    m_pFragmentShader.CopyFrom(pSrc.m_pFragmentShader, pSrc.m_pFragmentShader.Size);

    m_pModel.Assign(pSrc.m_pModel);
    m_pPreCalculatedLight.Assign(pSrc.m_pPreCalculatedLight);

    m_pMD2.Gesture := NativeUInt(m_Gesture);
end;
//--------------------------------------------------------------------------------------------------

end.
