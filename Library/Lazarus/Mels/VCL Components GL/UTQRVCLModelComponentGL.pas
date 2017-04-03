// *************************************************************************************************
// * ==> UTQRVCLModelComponentGL ------------------------------------------------------------------*
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
 @abstract(@name provides a basic model component using OpenGL to draw it.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRVCLModelComponentGL;

{$MODE Delphi}

interface

    // resources
    {$R UTQRVCLModelComponentGL.rc}

uses LCLType,
     Classes,
     SysUtils,
     Graphics,
     Controls,
     ExtCtrls,
     Forms,
     Windows,
     Messages,
     Win32Extra,
     Gl,
     GLext,
     UTQRDesignPatterns,
     UTQRGeometry,
     UTQR3D,
     UTQRCollision,
     UTQRModel,
     UTQRModelGroup,
     UTQRVCLAnimationTimer,
     UTQRVCLModelRendererGL,
     UTQRVCLModelShaderGL,
     UTQRVCLModelRenderSurfaceGL,
     UTQRVCLModelComponentPropertiesGL,
     UTQRVCLHelpers;

const
    // device caps
    {$EXTERNALSYM SHADEBLENDCAPS}
    SHADEBLENDCAPS = 45;
    {$EXTERNALSYM SB_NONE}
    SB_NONE = 0;

type
    {$REGION 'Documentation'}
    {**
     Antialiasing mode
     @value(EQR_AM_None No antialiasing enabled)
     @value(EQR_AM_FSAA_2x Enables a full screen antialiasing with a factor of 2)
     @value(EQR_AM_FSAA_4x Enables a full screen antialiasing with a factor of 4)
     @value(EQR_AM_FSAA_8x Enables a full screen antialiasing with a factor of 8)
    }
    {$ENDREGION}
    EQRAntialiasingMode =
    (
        EQR_AM_None    = 0,
        EQR_AM_FSAA_2x,
        EQR_AM_FSAA_4x,
        EQR_AM_FSAA_8x
    );

    {$REGION 'Documentation'}
    {**
     Called when OpenGL was created and should be configured
     @param(pSender Event sender)
     @param(hDC Device context)
     @param(hGLRC OpenGL render context)
     @param(pRenderer OpenGL renderer)
     @param(pShader OpenGL shader)
    }
    {$ENDREGION}
    TQRConfigureOpenGL = procedure(pSender: TObject;
                                hDC, hGLRC: THandle;
                                 pRenderer: TQRVCLModelRendererGL;
                                   pShader: TQRVCLModelShaderGL) of object;

    {$REGION 'Documentation'}
    {**
     Called when projection and view matrix are created
     @param(pSender - event sender)
     @param(projectionMatrix @bold([in, out]) Projection matrix to use)
     @param(viewMatrix @bold([in, out]) View matrix to use)
     @param(hDC Device context)
     @param(hGLRC OpenGL render context)
     @param(pRenderer OpenGL renderer)
     @param(pShader OpenGL shader)
     @return(@true to use user defined matrix instead of default, otherwise @false)
    }
    {$ENDREGION}
    TQRCreateSceneMatrix = function(pSender: TObject;
                       var projectionMatrix,
                                 viewMatrix: TQRMatrix4x4;
                                 hDC, hGLRC: THandle;
                                  pRenderer: TQRVCLModelRendererGL;
                                    pShader: TQRVCLModelShaderGL): Boolean of object;

    {$REGION 'Documentation'}
    {**
     Called when texture should be loaded
     @param(pSender Event sender)
     @param(hDC Device context)
     @param(hGLRC OpenGL render context)
     @param(pRenderer OpenGL renderer)
     @param(pShader OpenGL shader)
    }
    {$ENDREGION}
    TQRLoadTextureEvent = procedure(pSender: TObject;
                                 hDC, hGLRC: THandle;
                                  pRenderer: TQRVCLModelRendererGL;
                                    pShader: TQRVCLModelShaderGL) of object;

    {$REGION 'Documentation'}
    {**
     Called when a scene is initialized, before OpenGL clears the render target
     @param(pSender Event sender)
     @param(hDC Device context)
    }
    {$ENDREGION}
    TQRInitializeSceneEvent = procedure(pSender: TObject; hDC: THandle) of object;

    {$REGION 'Documentation'}
    {**
     Called before a scene is drawn
     @param(pSender Event sender)
     @param(hDC Device context)
     @param(hGLRC OpenGL render context)
     @param(pRenderer OpenGL renderer)
     @param(pShader OpenGL shader)
    }
    {$ENDREGION}
    TQRBeforeDrawSceneEvent = procedure(pSender: TObject;
                                     hDC, hGLRC: THandle;
                                      pRenderer: TQRVCLModelRendererGL;
                                        pShader: TQRVCLModelShaderGL) of object;

    {$REGION 'Documentation'}
    {**
     Called after a scene is drawn
     @param(pSender Event sender)
     @param(hDC Device context)
     @param(hGLRC OpenGL render context)
     @param(pRenderer OpenGL renderer)
     @param(pShader OpenGL shader)
    }
    {$ENDREGION}
    TQRAfterDrawSceneEvent = procedure(pSender: TObject;
                                    hDC, hGLRC: THandle;
                                     pRenderer: TQRVCLModelRendererGL;
                                       pShader: TQRVCLModelShaderGL) of object;

    {$REGION 'Documentation'}
    {**
     Called when a scene is finalized, after OpenGL has swapped the render target
     @param(pSender Event sender)
     @param(hDC Device context)
    }
    {$ENDREGION}
    TQRFinalizeSceneEvent = procedure(pSender: TObject; hDC: THandle) of object;

    {$REGION 'Documentation'}
    {**
     Called when collisions should be detected on the model
     @param(pSender Event sender)
     @param(projectionMatrix Projection (or word) matrix used to render the model)
     @param(projectionMatrix View (or camera) matrix used to render the model)
     @param(modelMatrix Model matrix)
     @param(pAABBTree Model aligned-axis bounding box tree)
     @param(pRenderer OpenGL renderer)
     @param(pShader OpenGL shader)
    }
    {$ENDREGION}
    TQRDetectCollisionsEvent = procedure(pSender: TObject;
                          const projectionMatrix,
                                      viewMatrix,
                                     modelMatrix: TQRMatrix4x4;
                                       pAABBTree: TQRAABBTree;
                                       pRenderer: TQRVCLModelRendererGL;
                                         pShader: TQRVCLModelShaderGL) of object;

    {$REGION 'Documentation'}
    {**
     Called when the default image should be drawn
     @param(pSender Event sender)
     @param(hDC Device context)
     @return(@true if default image was drawn, otherwise @false)
    }
    {$ENDREGION}
    TQRDrawDefaultImageEvent = function(pSender: TObject;
                                            hDC: THandle): Boolean of object;

    {$REGION 'Documentation'}
    {**
     Basic model component using the VCL and OpenGL to draw it
     @exclude(todo -cImprovement -oJean: Unfortunately the performances of this class are low, among
                                         other because of a poor usage of OpenGL. The base class should
                                         be instead TCustomOpenGLControl, from the LazOpenGLContext
                                         package, that provides already a basic OpenGL renderer, better
                                         than implemented in this class. In the future see how to use
                                         it, and replace current implementation by this one.)
    }
    {$ENDREGION}
    TQRVCLModelComponentGL = class(TWinControl, IQRObserver)
        private
            m_pDefaultImage:        TPortableNetworkGraphic;
            m_pColor:               TQRVCLModelComponentColorGL;
            m_pAlphaBlending:       TQRVCLModelComponentAlphaBlendingPropertyGL;
            m_pRenderer:            TQRVCLModelRendererGL;
            m_pShader:              TQRVCLModelShaderGL;
            m_pRenderSurface:       TQRVCLModelRenderSurfaceGL;
            m_pOverlay:             Graphics.TBitmap;
            m_hBackgroundBrush:     THandle;
            m_ViewMatrix:           TQRMatrix4x4;
            m_ProjectionMatrix:     TQRMatrix4x4;
            m_AntialiasingMode:     EQRAntialiasingMode;
            m_UseShader:            Boolean;
            m_SupportsGDI:          Boolean;
            m_LogMessageLoop:       Boolean;
            m_Allowed:              Boolean;
            m_Loaded:               Boolean;
            m_fOnConfigureOpenGL:   TQRConfigureOpenGL;
            m_fOnLoadTexture:       TQRLoadTextureEvent;
            m_fOnCreateSceneMatrix: TQRCreateSceneMatrix;
            m_fOnInitializeScene:   TQRInitializeSceneEvent;
            m_fOnBeforeDrawScene:   TQRBeforeDrawSceneEvent;
            m_fOnAfterDrawScene:    TQRAfterDrawSceneEvent;
            m_fOnFinalizeScene:     TQRFinalizeSceneEvent;
            m_fOnDetectCollisions:  TQRDetectCollisionsEvent;
            m_fOnDrawDefaultImage:  TQRDrawDefaultImageEvent;

        protected
            {$REGION 'Documentation'}
            {**
             Component Windows procedure
             @param(message Windows message)
            }
            {$ENDREGION}
            procedure WndProc(var message: TMessage); override;

            {$REGION 'Documentation'}
            {**
             Gets the view matrix
             @return(The view matrix)
            }
            {$ENDREGION}
            function GetViewMatrix: PQRMatrix4x4; virtual;

            {$REGION 'Documentation'}
            {**
             Sets the view matrix
             @param(pMatrix The view matrix)
            }
            {$ENDREGION}
            procedure SetViewMatrix(const pMatrix: PQRMatrix4x4);

            {$REGION 'Documentation'}
            {**
             Gets the projection matrix
             @return(The projection matrix)
            }
            {$ENDREGION}
            function GetProjectionMatrix: PQRMatrix4x4; virtual;

            {$REGION 'Documentation'}
            {**
             Sets the projection matrix
             @param(pMatrix The projection matrix)
            }
            {$ENDREGION}
            procedure SetProjectionMatrix(const pMatrix: PQRMatrix4x4);

            {$REGION 'Documentation'}
            {**
             Sets antialiasing mode
             @param(mode Antialiasing mode)
            }
            {$ENDREGION}
            procedure SetAntialiasingMode(mode: EQRAntialiasingMode); virtual;

            {$REGION 'Documentation'}
            {**
             Gets antialiasing factor
             @return(Antialiasing factor to apply)
            }
            {$ENDREGION}
            function GetAntialiasingFactor: NativeInt; virtual;

            {$REGION 'Documentation'}
            {**
             Sets if the model was successfully loaded and is ready to use
             @param (value If @true, the model was fully loaded and is ready to use)
            }
            {$ENDREGION}
            procedure SetModelLoaded(value: Boolean); virtual;

            {$REGION 'Documentation'}
            {**
             Called when control is resized
            }
            {$ENDREGION}
            procedure Resize; override;

            {$REGION 'Documentation'}
            {**
             Creates the control parameters
             @param(params Control parameters)
            }
            {$ENDREGION}
            procedure CreateParams(var params: TCreateParams); override;

            {$REGION 'Documentation'}
            {**
             Creates the component Windows handle
            }
            {$ENDREGION}
            procedure CreateHandle; override;

            {$REGION 'Documentation'}
            {**
             Deletes the component Windows handle
            }
            {$ENDREGION}
            procedure DestroyHandle; override;

            {$REGION 'Documentation'}
            {**
             Releases the draw context
            }
            {$ENDREGION}
            procedure ReleaseDrawContext; virtual;

            {$REGION 'Documentation'}
            {**
             Creates a viewport for the component
             @param(viewWidth Viewport width)
             @param(viewHeight Viewport height)
            }
            {$ENDREGION}
            procedure CreateViewport(viewWidth, viewHeight: NativeInt); virtual;

            {$REGION 'Documentation'}
            {**
             Builds shader
             @param(pVertexPrg Vertex shader program to compile and build)
             @param(pFragmentPrg Fragment shader program to compile and build)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function BuildShader(pVertexPrg, pFragmentPrg: TStream): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Draws a default image, in case a problem occurred while OpenGL library was initialized
            }
            {$ENDREGION}
            function DrawDefaultImage(hDC: THandle): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Draws the scene to the render surface
             @param(hDC Component device context)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function DrawSceneOnRenderSurface(hDC: THandle): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Draws the scene
             @param(hDC Internal control device context that OpenGL should use to draw the scene)
            }
            {$ENDREGION}
            procedure DrawScene(hDC: THandle); virtual;

            {$REGION 'Documentation'}
            {**
             Draws the scene to a device context
             @param(message Windows message containing device context to draw to)
             @return(@true if message was processed, otherwise @false)
            }
            {$ENDREGION}
            function DrawSceneTo(var message: TMessage): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Draws the scene to a device context
             @param(hDC Device context to draw the scene to)
             @param(x Position on the x axis where the scene will be drawn, in pixels)
             @param(y Position on the y axis where the scene will be drawn, in pixels)
            }
            {$ENDREGION}
            procedure DrawSceneTo(hDC: THandle; x, y: Integer); overload; virtual;

            {$REGION 'Documentation'}
            {**
             Called when OpenGL can be configured
            }
            {$ENDREGION}
            procedure OnConfigOpenGL; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Called when the scene content should be drawn
             @param(hDC Internal control device context that OpenGL should use to draw the scene)
            }
            {$ENDREGION}
            procedure OnDrawSceneContent(hDC: THandle); virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Receives and processes important messages from properties
             @param(pSender Event sender)
             @param(message Message to send to owner)
            }
            {$ENDREGION}
            function OnReceivePropNotification(pSender: TObject;
                                               message: EQRPropMessages): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Called when subject send a notification to the observer
             @param(message Notification message)
            }
            {$ENDREGION}
            procedure OnNotified(message: TQRMessage); virtual;

        // Properties
        protected
            {$REGION 'Documentation'}
            {**
             Gets the renderer
            }
            {$ENDREGION}
            property Renderer: TQRVCLModelRendererGL read m_pRenderer;

            {$REGION 'Documentation'}
            {**
             Gets the shader
            }
            {$ENDREGION}
            property Shader: TQRVCLModelShaderGL read m_pShader;

            {$REGION 'Documentation'}
            {**
             Gets the render surface
            }
            {$ENDREGION}
            property RenderSurface: TQRVCLModelRenderSurfaceGL read m_pRenderSurface;

            {$REGION 'Documentation'}
            {**
             Gets the view matrix
            }
            {$ENDREGION}
            property ViewMatrix: PQRMatrix4x4 read GetViewMatrix write SetViewMatrix;

            {$REGION 'Documentation'}
            {**
             Gets the projection matrix
            }
            {$ENDREGION}
            property ProjectionMatrix: PQRMatrix4x4 read GetProjectionMatrix write SetProjectionMatrix;

            {$REGION 'Documentation'}
            {**
             Gets the overlay
            }
            {$ENDREGION}
            property Overlay: Graphics.TBitmap read m_pOverlay;

            {$REGION 'Documentation'}
            {**
             Gets or sets if the drawing with the GDI is supported
            }
            {$ENDREGION}
            property SupportsGDI: Boolean read m_SupportsGDI write m_SupportsGDI;

            {$REGION 'Documentation'}
            {**
             Gets if shader are used
            }
            {$ENDREGION}
            property UseShader: Boolean read m_UseShader;

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
             Gets the control content in a bitmap
             @returns(Bitmap containing the scene, @nil on error)
             @br @bold(NOTE) The caller is respopnsible to free the bitmap when useless
             @br @bold(NOTE) The size of the returned bitmap is the client size multiplied by the
                             antialiasing factor. However the antialiasing is not applied
            }
            {$ENDREGION}
            function AsBitmap: Graphics.TBitmap;

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
             Gets if the model is allowed to work, i.e. if OpenGL was successfully initialized and
             is ready to use
            }
            {$ENDREGION}
            property IsAllowed: Boolean read m_Allowed;

            {$REGION 'Documentation'}
            {**
             Gets if the model was successfully loaded
             @br @bold(NOTE) This value is set to true only after the model was successfully loaded.
                             This means that this property may return false even in a normal
                             circumstance, and should never be interpreted as an error, but only as
                             an indication that the model is still not ready
            }
            {$ENDREGION}
            property IsLoaded: Boolean read m_Loaded;

            {$REGION 'Documentation'}
            {**
             Gets or sets if the message loop should be logged to the compiler console
             @br @bold(NOTE) This property is only allowed if library is compiled in debug mode
            }
            {$ENDREGION}
            {$IFDEF DEBUG}
                property LogMessageLoop: Boolean read m_LogMessageLoop write m_LogMessageLoop stored False;
            {$ENDIF}

        // Properties
        published
            {$REGION 'Documentation'}
            {**
             Gets or sets the model color
            }
            {$ENDREGION}
            property Color: TQRVCLModelComponentColorGL read m_pColor write m_pColor;

            {$REGION 'Documentation'}
            {**
             Gets or sets the alpha blending properties set
            }
            {$ENDREGION}
            property AlphaBlending: TQRVCLModelComponentAlphaBlendingPropertyGL read m_pAlphaBlending write m_pAlphaBlending;

            {$REGION 'Documentation'}
            {**
             Gets or sets the alpha blending mode, deactivated by default
            }
            {$ENDREGION}
            property Antialiasing: EQRAntialiasingMode read m_AntialiasingMode write SetAntialiasingMode default EQR_AM_None;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnConfigureOpenGL event
            }
            {$ENDREGION}
            property OnConfigureOpenGL: TQRConfigureOpenGL read m_fOnConfigureOpenGL write m_fOnConfigureOpenGL;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnLoadTexture event
            }
            {$ENDREGION}
            property OnLoadTexture: TQRLoadTextureEvent read m_fOnLoadTexture write m_fOnLoadTexture;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnCreateSceneMatrix event
            }
            {$ENDREGION}
            property OnCreateSceneMatrix: TQRCreateSceneMatrix read m_fOnCreateSceneMatrix write m_fOnCreateSceneMatrix;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnInitializeScene event
            }
            {$ENDREGION}
            property OnInitializeScene: TQRInitializeSceneEvent read m_fOnInitializeScene write m_fOnInitializeScene;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnBeforeDrawScene event
            }
            {$ENDREGION}
            property OnBeforeDrawScene: TQRBeforeDrawSceneEvent read m_fOnBeforeDrawScene write m_fOnBeforeDrawScene;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnAfterDrawScene event
            }
            {$ENDREGION}
            property OnAfterDrawScene: TQRAfterDrawSceneEvent read m_fOnAfterDrawScene write m_fOnAfterDrawScene;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnFinalizeScene event
            }
            {$ENDREGION}
            property OnFinalizeScene: TQRFinalizeSceneEvent read m_fOnFinalizeScene write m_fOnFinalizeScene;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnDetectCollisions event
            }
            {$ENDREGION}
            property OnDetectCollisions: TQRDetectCollisionsEvent read m_fOnDetectCollisions write m_fOnDetectCollisions;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnDrawDefaultImage event
            }
            {$ENDREGION}
            property OnDrawDefaultImage: TQRDrawDefaultImageEvent read m_fOnDrawDefaultImage write m_fOnDrawDefaultImage;

            {$REGION 'Documentation'}
            {**
             Gets or sets the component alignment
            }
            {$ENDREGION}
            property Align;

            {$REGION 'Documentation'}
            {**
             Gets or sets the component anchors
            }
            {$ENDREGION}
            property Anchors;

            {$REGION 'Documentation'}
            {**
             Gets or sets the compnent border spacing
            }
            {$ENDREGION}
            property BorderSpacing;

            {$REGION 'Documentation'}
            {**
             Gets or sets the component constraints
            }
            {$ENDREGION}
            property Constraints;

            {$REGION 'Documentation'}
            {**
             Gets or sets if the component is enabled
            }
            {$ENDREGION}
            property Enabled;

            {$REGION 'Documentation'}
            {**
             Gets or sets the popup menu to show when component is right clicked
            }
            {$ENDREGION}
            property PopupMenu;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnResize event
            }
            {$ENDREGION}
            property OnResize;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnContextPopup event
            }
            {$ENDREGION}
            property OnContextPopup;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnClick event
            }
            {$ENDREGION}
            property OnClick;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnDblClick event
            }
            {$ENDREGION}
            property OnDblClick;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnMouseDown event
            }
            {$ENDREGION}
            property OnMouseDown;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnMouseEnter event
            }
            {$ENDREGION}
            property OnMouseEnter;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnMouseLeave event
            }
            {$ENDREGION}
            property OnMouseLeave;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnMouseMove event
            }
            {$ENDREGION}
            property OnMouseMove;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnMouseUp event
            }
            {$ENDREGION}
            property OnMouseUp;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnEnter event
            }
            {$ENDREGION}
            property OnEnter;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnExit event
            }
            {$ENDREGION}
            property OnExit;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnKeyDown event
            }
            {$ENDREGION}
            property OnKeyDown;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnKeyPress event
            }
            {$ENDREGION}
            property OnKeyPress;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnKeyUp event
            }
            {$ENDREGION}
            property OnKeyUp;
    end;

    {$REGION 'Documentation'}
    {**
     Called when a static model item should be drawn on the scene
     @param(pSender Event sender)
     @param(hDC Device context)
     @param(hGLRC OpenGL render context)
     @param(pRenderer OpenGL renderer)
     @param(pShader OpenGL shader)
     @param(pGroup Group at which model belongs)
     @param(pModel Model to draw)
     @param(textures Textures belonging to model, in the order where they should be combined)
     @param(matrix Model matrix)
     @param(pMesh Mesh to draw, can be @nil (depends on selected options))
     @param(pAABBTree Aligned-axis bounding box tree matching with mesh, can be @nil (depends on
                      selected options))
     @return(@true if framed model item was drawn on the scene, otherwise @false)
    }
    {$ENDREGION}
    TQRDrawSceneStaticModelItemEvent = function (pSender: TObject;
                                              hDC, hGLRC: THandle;
                                               pRenderer: TQRVCLModelRendererGL;
                                                 pShader: TQRVCLModelShaderGL;
                                            const pGroup: TQRModelGroup;
                                            const pModel: TQRModel;
                                          const textures: TQRTextures;
                                            const matrix: TQRMatrix4x4;
                                             const pMesh: PQRMesh;
                                         const pAABBTree: TQRAABBTree): Boolean of object;

    {$REGION 'Documentation'}
    {**
     Basic model component for static models and using the VCL and OpenGL to draw it
    }
    {$ENDREGION}
    TQRVCLStaticModelComponentGL = class(TQRVCLModelComponentGL)
        private
            m_fDrawSceneStaticModelItemEvent: TQRDrawSceneStaticModelItemEvent;

        protected
            {$REGION 'Documentation'}
            {**
             Called when OpenGL can be configured
            }
            {$ENDREGION}
            procedure OnConfigOpenGL; override;

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
        published
            {$REGION 'Documentation'}
            {**
             Gets or sets the OnDrawSceneStaticModelItem event
            }
            {$ENDREGION}
            property OnDrawSceneStaticModelItem: TQRDrawSceneStaticModelItemEvent read m_fDrawSceneStaticModelItemEvent write m_fDrawSceneStaticModelItemEvent;
    end;

    {$REGION 'Documentation'}
    {**
     Called when a framed model item should be drawn on the scene
     @param(pSender Event sender)
     @param(hDC Device context)
     @param(hGLRC OpenGL render context)
     @param(pRenderer OpenGL renderer)
     @param(pShader OpenGL shader)
     @param(pGroup Group at which model belongs)
     @param(pModel Model to draw)
     @param(textures Textures belonging to model, in the order where they should be combined)
     @param(matrix Model matrix)
     @param(index Model mesh index)
     @param(nextIndex Model mesh index to interpolate with)
     @param(interpolationFactor Interpolation factor)
     @param(pMesh Mesh to draw, can be @nil (depends on selected options))
     @param(pNextMesh Next mesh to interpolate with, can be @nil (depends on selected options))
     @param(pAABBTree Aligned-axis bounding box tree matching with mesh, can be @nil (depends on
                      selected options))
     @param(pNextAABBTree Aligned-axis bounding box tree matching with next mesh, can be @nil
                          (depends on selected options))
     @return(@true if framed model item was drawn on the scene, otherwise @false)
    }
    {$ENDREGION}
    TQRDrawSceneFramedModelItemEvent = function (pSender: TObject;
                                              hDC, hGLRC: THandle;
                                               pRenderer: TQRVCLModelRendererGL;
                                                 pShader: TQRVCLModelShaderGL;
                                            const pGroup: TQRModelGroup;
                                            const pModel: TQRModel;
                                          const textures: TQRTextures;
                                            const matrix: TQRMatrix4x4;
                                        index, nextIndex: NativeInt;
                               const interpolationFactor: Double;
                                  const pMesh, pNextMesh: PQRMesh;
                          const pAABBTree, pNextAABBTree: TQRAABBTree): Boolean of object;

    {$REGION 'Documentation'}
    {**
     Basic model component supporting framed animation and using the VCL and OpenGL to draw it
    }
    {$ENDREGION}
    TQRVCLFramedModelComponentGL = class(TQRVCLModelComponentGL)
        private
            m_ElapsedTime:                    Double;
            m_NoAnimation:                    Boolean;
            m_fDrawSceneFramedModelItemEvent: TQRDrawSceneFramedModelItemEvent;

        protected
            {$REGION 'Documentation'}
            {**
             Called when OpenGL can be configured
            }
            {$ENDREGION}
            procedure OnConfigOpenGL; override;

        // Properties
        protected
            {$REGION 'Documentation'}
            {**
             Gets or sets the elapsed time since last drawn
            }
            {$ENDREGION}
            property ElapsedTime: Double read m_ElapsedTime write m_ElapsedTime;

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
             Called when subject send a notification to the observer
             @param(message Notification message)
            }
            {$ENDREGION}
            procedure OnNotified(message: TQRMessage); override;

            {$REGION 'Documentation'}
            {**
             Copies the property attributes from another property
             @param(pSource Source property to copy from)
            }
            {$ENDREGION}
            procedure Assign(pSource: TPersistent); override;

        // Properties
        published
            {$REGION 'Documentation'}
            {**
             Gets or sets if model should not be animated
            }
            {$ENDREGION}
            property NoAnimation: Boolean read m_NoAnimation write m_NoAnimation default False;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnDrawSceneFramedModelItem event
            }
            {$ENDREGION}
            property OnDrawSceneFramedModelItem: TQRDrawSceneFramedModelItemEvent read m_fDrawSceneFramedModelItemEvent write m_fDrawSceneFramedModelItemEvent;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVCLModelComponent
//--------------------------------------------------------------------------------------------------
constructor TQRVCLModelComponentGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // update control style to use
    ControlStyle := ControlStyle + [csReplicatable];
    ControlStyle := ControlStyle - [csOpaque, csFramed];

    // initialize local variables
    m_pDefaultImage        := TPortableNetworkGraphic.Create;
    m_pColor               := TQRVCLModelComponentColorGL.Create(Self, OnReceivePropNotification);
    m_pAlphaBlending       := TQRVCLModelComponentAlphaBlendingPropertyGL.Create(Self, OnReceivePropNotification);
    m_pRenderer            := TQRVCLModelRendererGL.Create;
    m_pShader              := TQRVCLModelShaderGL.Create;
    m_pRenderSurface       := TQRVCLModelRenderSurfaceGL.Create(Self, m_pRenderer);
    m_pOverlay             := nil;
    m_hBackgroundBrush     := 0;
    m_AntialiasingMode     := EQR_AM_None;
    m_UseShader            := False;
    m_SupportsGDI          := False;
    m_LogMessageLoop       := False;
    m_Allowed              := False;
    m_Loaded               := False;
    m_fOnConfigureOpenGL   := nil;
    m_fOnLoadTexture       := nil;
    m_fOnCreateSceneMatrix := nil;
    m_fOnInitializeScene   := nil;
    m_fOnBeforeDrawScene   := nil;
    m_fOnAfterDrawScene    := nil;
    m_fOnFinalizeScene     := nil;
    m_fOnDetectCollisions  := nil;
    m_fOnDrawDefaultImage  := nil;

    // configure some default properties
    DoubleBuffered := True;
    Width          := 100;
    Height         := 100;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLModelComponentGL.Destroy;
begin
    // clear memory. NOTE overlay structures are deleted while handle is destroyed
    m_pShader.Free;
    m_pAlphaBlending.Free;
    m_pColor.Free;
    m_pDefaultImage.Free;

    // delete background brush
    if (m_hBackgroundBrush <> 0) then
    begin
        DeleteObject(m_hBackgroundBrush);
        m_hBackgroundBrush := 0;
    end;

    // release draw context, if exists
    ReleaseDrawContext;

    // the render surface must ALWAYS be deleted after ReleaseDrawContext is called, otherwise some
    // access violations may happen while the draw context is destroyed. Also don't forget ot set it
    // to nil
    m_pRenderSurface.Free;
    m_pRenderSurface := nil;

    // the renderer must ALWAYS be deleted after ReleaseDrawContext is called, and after render
    // surface is deleted, otherwise some access violations may happen while the draw context is
    // destroyed. Also don't forget ot set it to nil
    m_pRenderer.Free;
    m_pRenderer := nil;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.WndProc(var message: TMessage);
var
    hDC:  THandle;
    ps:   TPaintStruct;
    rect: TRect;
begin
    {$IFDEF DEBUG}
        // log message loop, if activated
        if (m_LogMessageLoop) then
            TQRLogHelper.LogToCompiler(TQRLogHelper.WinMsgToStr(Self, message));
    {$ENDIF}

    // dispatch message
    case (message.Msg) of
        WM_ERASEBKGND:
        begin
            if (m_Allowed and m_Loaded) then
            begin
                // as scene background is always filled by OpenGL, ignore message to prevent ugly
                // flickering while scene is drawn. NOTE user is responsible to clear background before
                // drawing a transparent or translucent scene, by handling the OnInitializeScene event
                message.Result := 0;
                Exit;
            end;

            // get the device context to use
            hDC := message.WParam;

            // do use the provided device context?
            if ((hDC <> 0) and (m_hBackgroundBrush <> 0)) then
            begin
                rect.Left   := 0;
                rect.Top    := 0;
                rect.Right  := ClientWidth;
                rect.Bottom := ClientHeight;

                FillRect(hDC, rect, m_hBackgroundBrush);
                Exit;
            end;
        end;

        WM_PAINT:
        begin
            // handle is allocated and component isn't currently destroying?
            if (HandleAllocated and (not(csDestroying in ComponentState))) then
            begin
                // although this is not documented inside the MS documentation, sometimes the device
                // context to use may be sent inside the wParam property. If it's the case, use it
                hDC := message.WParam;

                // do use the provided device context?
                if (hDC <> 0) then
                    DrawScene(hDC)
                else
                    try
                        ps := Default(TPaintStruct);

                        // begin to paint the scene
                        hDC := BeginPaint(WindowHandle, ps);

                        // draw the scene
                        DrawScene(hDC);
                    finally
                        // end scene painting
                        EndPaint(WindowHandle, ps);
                    end;

                // validate entire client rect and exit (it has just been completely redrawn)
                ValidateRect(WindowHandle, nil);
                Exit;
            end;
        end;

        WM_PRINT:
        begin
            if (DrawSceneTo(message)) then
                Exit;
        end;

        WM_PRINTCLIENT:
        begin
            if (DrawSceneTo(message)) then
                Exit;
        end;
    end;

    inherited WndProc(message);
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelComponentGL.GetViewMatrix: PQRMatrix4x4;
begin
    Result := @m_ViewMatrix;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.SetViewMatrix(const pMatrix: PQRMatrix4x4);
begin
    m_ViewMatrix := pMatrix^;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelComponentGL.GetProjectionMatrix: PQRMatrix4x4;
begin
    Result := @m_ProjectionMatrix;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.SetProjectionMatrix(const pMatrix: PQRMatrix4x4);
begin
    m_ProjectionMatrix := pMatrix^;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.SetAntialiasingMode(mode: EQRAntialiasingMode);
begin
    // nothing to do?
    if (mode = m_AntialiasingMode) then
        Exit;

    m_AntialiasingMode := mode;

    RecreateWnd(Self);
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelComponentGL.GetAntialiasingFactor: NativeInt;
begin
    case (m_antialiasingMode) of
        EQR_AM_None:    Result := 1;
        EQR_AM_FSAA_2x: Result := 2;
        EQR_AM_FSAA_4x: Result := 4;
        EQR_AM_FSAA_8x: Result := 8;
    else
        raise Exception.CreateFmt('Unknown antialiasing mode - %d', [Integer(m_AntialiasingMode)]);
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.SetModelLoaded(value: Boolean);
begin
    m_Loaded := value;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.Resize;
begin
    inherited Resize;

    // resize viewport
    CreateViewport(ClientWidth, ClientHeight);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.CreateParams(var params: TCreateParams);
begin
    inherited CreateParams(params);

    // update Windows class to update automatically the paint every time the control is resized
    params.WindowClass.Style := params.WindowClass.style or CS_HREDRAW or CS_VREDRAW;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.CreateHandle;
var
    hDC:    THandle;
    factor: NativeInt;
begin
    // release previous draw context, if exists
    ReleaseDrawContext;

    inherited CreateHandle;

    // check if handle was successfully allocated
    if (not HandleAllocated) then
        Exit;

    // get the device context for this control
    hDC := GetDC(WindowHandle);

    // found it?
    if (hDC = 0) then
        Exit;

    try
        factor := GetAntialiasingFactor;

        // initialize new render surface instance
        m_Allowed := m_pRenderSurface.Initialize(factor,
                                                 m_pAlphaBlending.Enabled,
                                                 m_SupportsGDI);

        // create and configure local overlay
        m_pOverlay             := Graphics.TBitmap.Create;
        m_pOverlay.PixelFormat := pf32bit;

        // configure OpenGL
        OnConfigOpenGL;

        // notify user that OpenGL can be configured
        if (m_Allowed and Assigned(m_fOnConfigureOpenGL)) then
            m_fOnConfigureOpenGL(Self,
                                 hDC,
                                 m_pRenderSurface.GLContext,
                                 m_pRenderer,
                                 m_pShader);

        // create background brush
        m_hBackgroundBrush := CreateSolidBrush(ColorToRGB(Color.VCLColor));

        // create a viewport for the scene
        CreateViewport(ClientWidth, ClientHeight);
    finally
        ReleaseDC(WindowHandle, hDC);
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.DestroyHandle;
begin
    // delete background brush
    if (m_hBackgroundBrush <> 0) then
    begin
        DeleteObject(m_hBackgroundBrush);
        m_hBackgroundBrush := 0;
    end;

    // release draw context, if exists
    ReleaseDrawContext;

    inherited DestroyHandle;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.ReleaseDrawContext;
begin
    // clear overlay
    m_pOverlay.Free;
    m_pOverlay := nil;

    // check if render surface is still assigned
    if (Assigned(m_pRenderSurface)) then
        // release render surface instance, if exists
        m_pRenderSurface.Release;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.CreateViewport(viewWidth, viewHeight: NativeInt);
var
    factor:                  NativeInt;
    position, direction, up: TQRVector3D;
    hDC:                     THandle;
begin
    // cannot create a viewport if there is no client surface to render to it
    if ((viewWidth = 0) or (viewHeight = 0)) then
        Exit;

    // no render surface?
    if (not Assigned(m_pRenderSurface)) then
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
        if (not m_pRenderSurface.EnableContext) then
            Exit;

        // resize render surface
        m_pRenderSurface.Resize;

        // resize local overlay, if any
        if (Assigned(m_pOverlay)) then
            m_pOverlay.SetSize(viewWidth, viewHeight);

        // OpenGL was initialized correctly?
        if (m_Allowed) then
        begin
            // get antialiasing factor to apply
            factor := GetAntialiasingFactor;

            // create OpenGL viewport to use to draw scene
            m_pRenderer.CreateViewport(viewWidth * factor, viewHeight * factor);

            // notify user that scene matrix (i.e. projection and view matrix) are about to be created
            if (Assigned(m_fOnCreateSceneMatrix)) then
                // user defined his own matrix?
                if (m_fOnCreateSceneMatrix(Self,
                                           m_ProjectionMatrix,
                                           m_ViewMatrix,
                                           hDC,
                                           RenderSurface.GLContext,
                                           m_pRenderer,
                                           m_pShader))
                then
                    Exit;

            // create projection matrix (will not be modified while execution)
            m_ProjectionMatrix := m_pRenderer.GetOrtho(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);

            position  := TQRVector3D.Create(0.0, 0.0, 0.0);
            direction := TQRVector3D.Create(0.0, 0.0, 1.0);
            up        := TQRVector3D.Create(0.0, 1.0, 0.0);

            // create view matrix (will not be modified while execution)
            m_ViewMatrix := m_pRenderer.LookAtLH(position, direction, up);

            // do use shader?
            if (not m_UseShader) then
            begin
                // load projection matrix and initialize it
                glMatrixMode(GL_PROJECTION);
                glLoadIdentity;

                // apply projection matrix
                glLoadMatrixf(PGLfloat(m_ProjectionMatrix.GetPtr));

                // load model view matrix and initialize it
                glMatrixMode(GL_MODELVIEW);
                glLoadIdentity;

                // apply model view matrix
                glLoadMatrixf(PGLfloat(m_ViewMatrix.GetPtr));
            end;
        end;
    finally
        ReleaseDC(WindowHandle, hDC);
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelComponentGL.BuildShader(pVertexPrg, pFragmentPrg: TStream): Boolean;
begin
    // OpenGL was not initialized correctly?
    if (not m_Allowed) then
        Exit(False);

    // load and compile shader
    m_pShader.CreateProgram;
    m_pShader.AttachFile(pVertexPrg,   EQR_ST_Vertex);
    m_pShader.AttachFile(pFragmentPrg, EQR_ST_Fragment);

    // try to link shader
    Result := m_pShader.Link(False);
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelComponentGL.DrawDefaultImage(hDC: THandle): Boolean;
var
    hPackageInstance: NativeUInt;
    pStream:          TResourceStream;
    pBitmap:          Graphics.TBitmap;
    rect:             TRect;
    bf:               TBlendFunction;
begin
    // is component currently deleting?
    if (csDestroying in ComponentState) then
        Exit(False);

    // no device context to draw to?
    if (hDC = 0) then
        Exit(False);

    // fill control background (to avoid transparency error in case the scene draw failed)
    if (m_hBackgroundBrush <> 0) then
    begin
        rect.Left   := 0;
        rect.Top    := 0;
        rect.Right  := ClientWidth;
        rect.Bottom := ClientHeight;

        FillRect(hDC, rect, m_hBackgroundBrush);
    end;

    pStream := nil;

    // is default image still not loaded?
    if ((m_pDefaultImage.Width = 0) or (m_pDefaultImage.Height = 0)) then
        try
            // get module instance at which this control belongs
            hPackageInstance := System.HInstance;

            // found module and package contains the default image?
            if ((hPackageInstance = 0) or
                (FindResource(hPackageInstance, PChar('RC_BROKEN_MODEL_IMAGE'), RT_RCDATA) = 0))
            then
                Exit(False);

            // load normals table from stream
            pStream := TResourceStream.Create(hPackageInstance,
                                              PChar('RC_BROKEN_MODEL_IMAGE'),
                                              RT_RCDATA);

            // load default image from stream
            m_pDefaultImage.LoadFromStream(pStream);
        finally
            // delete resource stream, if needed
            pStream.Free
        end;

    pBitmap := nil;

    try
        // create a new bitmap image
        pBitmap             := Graphics.TBitmap.Create;
        pBitmap.PixelFormat := pf32bit;
        pBitmap.SetSize(m_pDefaultImage.Width, m_pDefaultImage.Height);

        rect.Left   := 0;
        rect.Top    := 0;
        rect.Right  := m_pDefaultImage.Width;
        rect.Bottom := m_pDefaultImage.Height;

        // clear background to make it transparent
        pBitmap.Canvas.Brush.Style := bsClear;
        pBitmap.Canvas.Brush.Color := Color.VCLColor;
        pBitmap.Canvas.FillRect(rect);

        // copy default image into bitmap
        pBitmap.Canvas.Draw(0, 0, m_pDefaultImage);

        // configure alpha blending operation
        bf.BlendOp             := AC_SRC_OVER;
        bf.BlendFlags          := 0;
        bf.SourceConstantAlpha := 255;
        bf.AlphaFormat         := AC_SRC_ALPHA;

        // draw default image
        if ((ClientWidth >= 100) and (ClientHeight >= 100)) then
            AlphaBlend(hDC,
                      (ClientWidth  shr 1) - (m_pDefaultImage.Width  shr 1),
                      (ClientHeight shr 1) - (m_pDefaultImage.Height shr 1),
                       m_pDefaultImage.Width,
                       m_pDefaultImage.Height,
                       pBitmap.Canvas.Handle,
                       0,
                       0,
                       m_pDefaultImage.Width,
                       m_pDefaultImage.Height,
                       bf)
        else
            AlphaBlend(hDC,
                       0,
                       0,
                       ClientWidth,
                       ClientHeight,
                       pBitmap.Canvas.Handle,
                       0,
                       0,
                       m_pDefaultImage.Width,
                       m_pDefaultImage.Height,
                       bf);
    finally
        pBitmap.Free;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelComponentGL.DrawSceneOnRenderSurface(hDC: THandle): Boolean;
begin
    // resize render surface to match with current component size
    m_pRenderSurface.Resize;

    // begin to draw the scene
    if (not m_pRenderSurface.BeginScene) then
        Exit(False);

    // clear the scene background
    glClearColor(m_pColor.RedF, m_pColor.GreenF, m_pColor.BlueF, m_pColor.AlphaF);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    // notify user that scene is about to be drawn
    if (Assigned(m_fOnBeforeDrawScene)) then
        m_fOnBeforeDrawScene(Self,
                             hDC,
                             m_pRenderSurface.GLContext,
                             m_pRenderer,
                             m_pShader);

    // draw the scene
    OnDrawSceneContent(hDC);

    // notify user that scene is drawn
    if (Assigned(m_fOnAfterDrawScene)) then
        m_fOnAfterDrawScene(Self,
                            hDC,
                            m_pRenderSurface.GLContext,
                            m_pRenderer,
                            m_pShader);

    // end the scene on render surface
    m_pRenderSurface.EndScene;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.DrawScene(hDC: THandle);
var
    bf:                                            TBlendFunction;
    rect:                                          TRect;
    factor, antialiasingWidth, antialiasingHeight: NativeInt;
begin
    // check if handle was already created, create it if not
    HandleNeeded;

    // no received device context to draw to?
    if (hDC = 0) then
        Exit;

    try
        // not allowed to draw the scene? (i.e. OpenGL was not initialized correctly)
        if (not m_Allowed) then
        begin
            // allow user to draw his own default image
            if (Assigned(m_fOnDrawDefaultImage)) then
                if (m_fOnDrawDefaultImage(Self, hDC)) then
                    Exit;

            DrawDefaultImage(hDC);
            Exit;
        end;

        // for the design time, paint the background in black to avoid visual artifacts if alpha
        // blending is enabled
        if ((m_pAlphaBlending.Enabled)      and
            (csDesigning in ComponentState) and
            (m_hBackgroundBrush <> 0))
        then
        begin
            rect.Left   := 0;
            rect.Top    := 0;
            rect.Right  := ClientWidth;
            rect.Bottom := ClientHeight;

            FillRect(hDC, rect, m_hBackgroundBrush);
        end;

        // resize antialiasing overlay, if any
        if (not Assigned(m_pOverlay)) then
        begin
            // allow user to draw his own default image
            if (Assigned(m_fOnDrawDefaultImage)) then
                if (m_fOnDrawDefaultImage(Self, hDC)) then
                    Exit;

            DrawDefaultImage(hDC);
            Exit;
        end;

        // clear overlay and make it transparent
        m_pOverlay.Canvas.Brush.Style := bsClear;
        m_pOverlay.Canvas.Brush.Color := Color.VCLColor;
        m_pOverlay.Canvas.FillRect(rect);

        factor             := GetAntialiasingFactor;
        antialiasingWidth  := ClientWidth  * factor;
        antialiasingHeight := ClientHeight * factor;

        // overlay size changed since last draw?
        if ((m_pOverlay.Width  <> antialiasingWidth) or
            (m_pOverlay.Height <> antialiasingHeight))
        then
            // update overlay size
            m_pOverlay.SetSize(antialiasingWidth, antialiasingHeight);

        // begin to draw scene on overlay surface
        if (not DrawSceneOnRenderSurface(hDC)) then
        begin
            // allow user to draw his own default image
            if (Assigned(m_fOnDrawDefaultImage)) then
                if (m_fOnDrawDefaultImage(Self, hDC)) then
                    Exit;

            DrawDefaultImage(hDC);
            Exit;
        end;

        try
            m_pOverlay.BeginUpdate;

            // get drawn scene as bitmap
            m_pRenderSurface.GetBitmap(m_pOverlay);
        finally
            m_pOverlay.EndUpdate;
        end;

        // notify user that scene is initialized and ready to be drawn. NOTE the scene is
        // initialized after OpenGL painted it, because an overlay was used in this case, and
        // initializing scene just before copying overlay reduce flickering
        if (m_SupportsGDI and Assigned(m_fOnInitializeScene)) then
            m_fOnInitializeScene(Self, hDC);

        if (AlphaBlending.Enabled and (AlphaBlending.GlobalLevel <> 255)) then
        begin
            // configure alpha blending operation
            bf.BlendOp             := AC_SRC_OVER;
            bf.BlendFlags          := 0;
            bf.SourceConstantAlpha := AlphaBlending.GlobalLevel;
            bf.AlphaFormat         := AC_SRC_ALPHA;

            // apply antialiasing
            AlphaBlend(hDC,
                       0,
                       0,
                       ClientWidth,
                       ClientHeight,
                       m_pOverlay.Canvas.Handle,
                       0,
                       0,
                       m_pOverlay.Width,
                       m_pOverlay.Height,
                       bf);
        end
        else
        if (factor <> 1) then
            // apply antialiasing
            TQRGDIHelper.ApplyAntialiasing(m_pOverlay.Canvas.Handle,
                                           hDC,
                                           0,
                                           0,
                                           ClientWidth,
                                           ClientHeight,
                                           factor)
        else
            // copy scene from overlay to final device context
            BitBlt(hDC,
                   0,
                   0,
                   ClientWidth,
                   ClientHeight,
                   m_pOverlay.Canvas.Handle,
                   0,
                   0,
                   SRCCOPY);
    finally
        // notify user that scene is completely drawn
        if (m_SupportsGDI and Assigned(m_fOnFinalizeScene)) then
            m_fOnFinalizeScene(Self, hDC);
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelComponentGL.DrawSceneTo(var message: TMessage): Boolean;
var
    hDC, hBackgroundBrush: THandle;
    rect:                  TRect;
begin
    // is component currently destroying?
    if (csDestroying in ComponentState) then
        Exit(False);

    hDC := THandle(message.WParam);

    if (hDC = 0) then
        Exit(False);

    // do check if control is visible before draw scene to device context?
    if ((message.LParam and PRF_CHECKVISIBLE) <> 0) then
        if (not Visible) then
            Exit(False);

    // do erase background before draw the scene to the device context?
    if ((message.LParam and PRF_ERASEBKGND) <> 0) then
    begin
        hBackgroundBrush := 0;

        try
            // create a brush to paint the background
            hBackgroundBrush := CreateSolidBrush(ColorToRGB(Color.VCLColor));

            // succeeded?
            if (hBackgroundBrush <> 0) then
            begin
                rect.Left   := 0;
                rect.Top    := 0;
                rect.Right  := ClientWidth;
                rect.Bottom := ClientHeight;

                // paint the background
                FillRect(hDC, rect, hBackgroundBrush);
            end;
        finally
            // clear memory
            if (hBackgroundBrush <> 0) then
                DeleteObject(hBackgroundBrush);
        end;
    end;

    // draw the scene
    DrawSceneTo(hDC, 0, 0);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.DrawSceneTo(hDC: THandle; x, y: Integer);
var
    rect:                                          TRect;
    factor, antialiasingWidth, antialiasingHeight: NativeInt;
begin
    // no received device context to draw to?
    if (hDC = 0) then
        Exit;

    try
        // not allowed to draw the scene? (i.e. OpenGL was not initialized correctly)
        if (not m_Allowed) then
        begin
            // allow user to draw his own default image
            if (Assigned(m_fOnDrawDefaultImage)) then
                if (m_fOnDrawDefaultImage(Self, hDC)) then
                    Exit;

            DrawDefaultImage(hDC);
            Exit;
        end;

        // for the design time, paint the background in black to avoid visual artifacts if alpha
        // blending is enabled
        if ((m_pAlphaBlending.Enabled)      and
            (csDesigning in ComponentState) and
            (m_hBackgroundBrush <> 0))
        then
        begin
            rect.Left   := 0;
            rect.Top    := 0;
            rect.Right  := ClientWidth;
            rect.Bottom := ClientHeight;

            FillRect(hDC, rect, m_hBackgroundBrush);
        end;

        // resize antialiasing overlay, if any
        if (not Assigned(m_pOverlay)) then
        begin
            // allow user to draw his own default image
            if (Assigned(m_fOnDrawDefaultImage)) then
                if (m_fOnDrawDefaultImage(Self, hDC)) then
                    Exit;

            DrawDefaultImage(hDC);
            Exit;
        end;

        // clear overlay and make it transparent
        m_pOverlay.Canvas.Brush.Style := bsClear;
        m_pOverlay.Canvas.Brush.Color := Color.VCLColor;
        m_pOverlay.Canvas.FillRect(rect);

        factor             := GetAntialiasingFactor;
        antialiasingWidth  := ClientWidth  * factor;
        antialiasingHeight := ClientHeight * factor;

        // overlay size changed since last draw?
        if ((m_pOverlay.Width  <> antialiasingWidth) or (m_pOverlay.Height <> antialiasingHeight))
        then
            // update overlay size
            m_pOverlay.SetSize(antialiasingWidth, antialiasingHeight);

        // begin to draw scene on overlay surface
        if (not DrawSceneOnRenderSurface(hDC)) then
        begin
            // allow user to draw his own default image
            if (Assigned(m_fOnDrawDefaultImage)) then
                if (m_fOnDrawDefaultImage(Self, hDC)) then
                    Exit;

            DrawDefaultImage(hDC);
            Exit;
        end;

        try
            m_pOverlay.BeginUpdate;

            // get drawn scene as bitmap
            m_pRenderSurface.GetBitmap(m_pOverlay);
        finally
            m_pOverlay.EndUpdate;
        end;

        // notify user that scene is initialized and ready to be drawn. NOTE the scene is
        // initialized after OpenGL painted it, because an overlay was used in this case, and
        // initializing scene just before copying overlay reduce flickering
        if (Assigned(m_fOnInitializeScene)) then
            m_fOnInitializeScene(Self, hDC);

        // copy scene from overlay to final device context
        BitBlt(hDC,
               0,
               0,
               ClientWidth,
               ClientHeight,
               m_pOverlay.Canvas.Handle,
               0,
               0,
               SRCCOPY);
    finally
        // notify user that scene is completely drawn
        if (Assigned(m_fOnFinalizeScene)) then
            m_fOnFinalizeScene(Self, hDC);
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelComponentGL.OnReceivePropNotification(pSender: TObject;
                                                          message: EQRPropMessages): Boolean;
begin
    // dispatch message
    case (message) of
        EQR_PM_RecreateWnd: RecreateWnd(Self);
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.OnNotified(message: TQRMessage);
begin
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelComponentGL.AsBitmap: Graphics.TBitmap;
var
    success: Boolean;
begin
    Result  := nil;
    success := False;

    try
        // create and configure resulting bitmap
        Result             := Graphics.TBitmap.Create;
        Result.PixelFormat := pf32bit;
        Result.SetSize(ClientWidth, ClientHeight);

        // draw the scene
        DrawSceneTo(Result.Canvas.Handle, 0, 0);

        // copy the scene to result bitmap
        Result.Assign(m_pOverlay);

        success := True;
    finally
        if (not success) then
        begin
            Result.Free;
            Result := nil;
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLModelComponentGL.Assign(pSource: TPersistent);
var
    pSrc: TQRVCLModelComponentGL;
begin
    inherited Assign(pSource);

    // incorrect source type?
    if (not(pSource is TQRVCLModelComponentGL)) then
    begin
        // reset values to default
        m_LogMessageLoop := False;

        m_pColor.Assign(nil);
        m_pAlphaBlending.Assign(nil);
        Exit;
    end;

    // copy content from source
    pSrc             := pSource as TQRVCLModelComponentGL;
    m_LogMessageLoop := pSrc.m_LogMessageLoop;

    m_pColor.Assign(pSrc.m_pColor);
    m_pAlphaBlending.Assign(pSrc.m_pAlphaBlending);
end;
//--------------------------------------------------------------------------------------------------
// TQRVCLStaticModelComponentGL
//--------------------------------------------------------------------------------------------------
constructor TQRVCLStaticModelComponentGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // initialize variables
    m_fDrawSceneStaticModelItemEvent := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLStaticModelComponentGL.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLStaticModelComponentGL.OnConfigOpenGL;
var
    hDC: THandle;
begin
    // OpenGL was not initialized correctly?
    if (not m_Allowed) then
        Exit;

    // enable and configure depth testing
    glEnable(GL_DEPTH_TEST);

    // enable and configure culling
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    // enable and configure texture rendering
    glEnable(GL_TEXTURE_2D);

    // notify that optional OpenGL configuration can be enabled
    if (Assigned(m_fOnConfigureOpenGL)) then
    begin
        // a Windows handle is required to create a device context
        HandleNeeded;

        // failed to create Windows handle?
        if (not HandleAllocated) then
            Exit;

        // get the device context for this control
        hDC := GetDC(WindowHandle);

        try
            m_fOnConfigureOpenGL(Self,
                                 hDC,
                                 m_pRenderSurface.GLContext,
                                 m_pRenderer,
                                 m_pShader);
        finally
            // free the device context
            ReleaseDC(WindowHandle, hDC)
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLStaticModelComponentGL.Assign(pSource: TPersistent);
begin
    inherited Assign(pSource);
end;
//--------------------------------------------------------------------------------------------------
// TQRVCLAnimatedModelComponentGL
//--------------------------------------------------------------------------------------------------
constructor TQRVCLFramedModelComponentGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // initialize values
    m_ElapsedTime                    := 0.0;
    m_NoAnimation                    := False;
    m_fDrawSceneFramedModelItemEvent := nil;

    // attach to animation timer to receive time notifications (runtime only)
    if (not(csDesigning in ComponentState)) then
        TQRVCLAnimationTimer.GetInstance.Attach(Self);
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLFramedModelComponentGL.Destroy;
begin
    // detach from animation timer and stop to receive time notifications (runtime only)
    if (not(csDesigning in ComponentState)) then
        TQRVCLAnimationTimer.GetInstance.Detach(Self);

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLFramedModelComponentGL.OnConfigOpenGL;
var
    hDC: THandle;
begin
    // OpenGL was not initialized correctly?
    if (not m_Allowed) then
        Exit;

    // enable and configure depth testing
    glEnable(GL_DEPTH_TEST);

    // enable and configure culling
    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);

    // enable and configure texture rendering
    glEnable(GL_TEXTURE_2D);

    // notify that optional OpenGL configuration can be enabled
    if (Assigned(m_fOnConfigureOpenGL)) then
    begin
        // a Windows handle is required to create a device context
        HandleNeeded;

        // failed to create Windows handle?
        if (not HandleAllocated) then
            Exit;

        // get the device context for this control
        hDC := GetDC(WindowHandle);

        try
            m_fOnConfigureOpenGL(Self,
                                 hDC,
                                 m_pRenderSurface.GLContext,
                                 m_pRenderer,
                                 m_pShader);
        finally
            // free the device context
            ReleaseDC(WindowHandle, hDC)
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLFramedModelComponentGL.OnNotified(message: TQRMessage);
var
    info: TQRVCLAnimationTimerMsgInfo;
    hDC:  THandle;
begin
    inherited OnNotified(message);

    // received a message from animation timer?
    case (EQRVCLAnimationTimerMessages(message.m_Type)) of
        EQR_AM_Animate:
        begin
            // not allowed to draw the scene? (Stop here to prevent the default image to flick)
            if (not m_Allowed) then
                Exit;

            // don't animate?
            if (m_NoAnimation) then
                Exit;

            // check if handle was successfully allocated
            if (not HandleAllocated) then
                Exit;

            // do nothing if the control isn't visible
            if ((not Visible) or (not IsWindowVisible(WindowHandle))) then
                Exit;

            // get elapsed time from message info sructure
            info          := TQRVCLAnimationTimerMsgInfo(message.m_pInfo^);
            m_ElapsedTime := info.m_ElapsedTime;

            // get the device context for this control
            hDC := GetDC(WindowHandle);

            // found it?
            if (hDC = 0) then
                Exit;

            try
                DrawScene(hDC);
            finally
                ReleaseDC(WindowHandle, hDC);
            end;
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLFramedModelComponentGL.Assign(pSource: TPersistent);
begin
    inherited Assign(pSource);
end;
//--------------------------------------------------------------------------------------------------

end.
