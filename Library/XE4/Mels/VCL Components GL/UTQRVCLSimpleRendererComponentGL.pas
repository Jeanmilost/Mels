{**************************************************************************************************
 * ==> UTQRVCLSimpleRendererComponentGL ----------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides a simple renderer that OpenGL can use to draw on it         *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRVCLSimpleRendererComponentGL;

interface

uses System.Classes,
     UTQRVCLModelRendererGL,
     UTQRVCLModelShaderGL,
     UTQRVCLModelComponentGL,
     UTQRVCLModelRenderSurfaceGL,
     Winapi.Messages,
     Winapi.Windows;

type
    {**
    * Called when a scene is drawn
    *@param pSender - event sender
    *@param hDC - device context
    *@param hGLRC - OpenGL render context
    *@param pRenderer - OpenGL renderer
    *@param pShader - OpenGL shader
    *}
    TQRDrawSceneEvent = procedure(pSender: TObject;
                               hDC, hGLRC: THandle;
                                pRenderer: TQRVCLModelRendererGL;
                                  pShader: TQRVCLModelShaderGL) of object;

    {**
    * Simple renderer component that OpenGL can use to draw on it
    *}
    TQRVCLSimpleRendererGL = class(TQRVCLModelComponentGL)
        protected
            m_fOnDrawScene: TQRDrawSceneEvent;

            {**
            * Called when OpenGL can be configured
            *}
            procedure OnConfigOpenGL; override;

            {**
            * Called when the scene content should be drawn
            *@param hDC - internal control device context that OpenGL should use to draw the scene
            *}
            procedure OnDrawSceneContent(hDC: THandle); override;

        public
            {**
            * Constructor
            *@param pOwner - component owner
            *}
            constructor Create(pOwner: TComponent); override;

            {**
            * Destructor
            *}
            destructor Destroy; override;

        published
            { Published properties }
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
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLSimpleRendererGL.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLSimpleRendererGL.OnConfigOpenGL;
var
    hDC: THandle;
begin
    // OpenGL was not initialized correctly?
    if (not Allowed) then
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
