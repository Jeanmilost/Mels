// *************************************************************************************************
// * ==> UTQRVCLSimpleRendererComponentGL ---------------------------------------------------------*
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
 @abstract(@name provides a simple renderer that OpenGL can use to draw on it.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRVCLSimpleRendererComponentGL;

interface

uses System.Classes,
     Winapi.Messages,
     Winapi.Windows,
     UTQRVCLModelRendererGL,
     UTQRVCLModelShaderGL,
     UTQRVCLModelComponentGL,
     UTQRVCLModelRenderSurfaceGL;

type
    {$REGION 'Documentation'}
    {**
     Called when a scene is drawn
     @param(pSender Event sender)
     @param(hDC Device context)
     @param(hGLRC OpenGL render context)
     @param(pRenderer OpenGL renderer)
     @param(pShader OpenGL shader)
    }
    {$ENDREGION}
    TQRDrawSceneEvent = procedure(pSender: TObject;
                               hDC, hGLRC: THandle;
                                pRenderer: TQRVCLModelRendererGL;
                                  pShader: TQRVCLModelShaderGL) of object;

    {$REGION 'Documentation'}
    {**
     Simple renderer component that OpenGL can use to draw on it
    }
    {$ENDREGION}
    TQRVCLSimpleRendererGL = class(TQRVCLModelComponentGL)
        private
            m_fOnDrawScene: TQRDrawSceneEvent;

        protected
            {$REGION 'Documentation'}
            {**
             Gets the supports GDI flag
             @return(The supports GDI flag state)
            }
            {$ENDREGION}
            function GetSupportsGDI: Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Sets the supports GDI flag
             @param(value The supports GDI flag state)
            }
            {$ENDREGION}
            procedure SetSupportsGDI(value: Boolean); virtual;

            {$REGION 'Documentation'}
            {**
             Called when OpenGL can be configured
            }
            {$ENDREGION}
            procedure OnConfigOpenGL; override;

            {$REGION 'Documentation'}
            {**
             Called when the scene content should be drawn
             @param(hDC Internal control device context that OpenGL should use to draw the scene)
            }
            {$ENDREGION}
            procedure OnDrawSceneContent(hDC: THandle); override;

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

        // Properties
        published
            {$REGION 'Documentation'}
            {**
             Gets or sets if the drawing with the GDI is supported
            }
            {$ENDREGION}
            property SupportsGDI: Boolean read GetSupportsGDI write SetSupportsGDI default False;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnDrawScene event
            }
            {$ENDREGION}
            property OnDrawScene: TQRDrawSceneEvent read m_fOnDrawScene write m_fOnDrawScene;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVCLSimpleRendererGL
//--------------------------------------------------------------------------------------------------
constructor TQRVCLSimpleRendererGL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    m_fOnDrawScene := nil;

    // nothing to load because it's done by the user
    SetModelLoaded(True);
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLSimpleRendererGL.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLSimpleRendererGL.GetSupportsGDI: Boolean;
begin
    Result := inherited SupportsGDI;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLSimpleRendererGL.SetSupportsGDI(value: Boolean);
begin
    // nothing to do?
    if (inherited SupportsGDI = value) then
        Exit;

    inherited SupportsGDI := value;

    RecreateWnd;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLSimpleRendererGL.OnConfigOpenGL;
var
    hDC: THandle;
begin
    // OpenGL was not initialized correctly?
    if (not IsAllowed) then
        Exit;

    // notify that optional OpenGL configuration can be enabled
    if (Assigned(OnConfigureOpenGL)) then
    begin
        // a Windows handle is required to create a device context
        HandleNeeded;

        // failed to create Windows handle?
        if (not HandleAllocated) then
            Exit;

        // get the device context for this control
        hDC := GetDC(WindowHandle);

        try
            OnConfigureOpenGL(Self,
                              hDC,
                              RenderSurface.GLContext,
                              Renderer,
                              Shader);
        finally
            // free the device context
            ReleaseDC(WindowHandle, hDC)
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLSimpleRendererGL.OnDrawSceneContent(hDC: Thandle);
begin
    // notify user that scene should be drawn
    if (Assigned(m_fOnDrawScene)) then
        m_fOnDrawScene(Self,
                       hDC,
                       RenderSurface.GLContext,
                       Renderer,
                       Shader);
end;
//--------------------------------------------------------------------------------------------------

end.
