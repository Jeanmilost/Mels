// *************************************************************************************************
// * ==> UTQRLight --------------------------------------------------------------------------------*
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
 @abstract(@name provides a lighting system for the 3D models.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRLight;

interface

uses UTQRGeometry,
     UTQRGraphics;

type
    {$REGION 'Documentation'}
    {**
     Basic light
    }
    {$ENDREGION}
    TQRLight = class
        private
            {$REGION 'Documentation'}
            {**
             Ambient color
             @br @bold(NOTE) The ambient color is the color of all meshes that are not illuminated
                             by another light. This can be compared e.g. to a room where no light
                             source are visible
            }
            {$ENDREGION}
            m_pAmbient: TQRColor;

            {$REGION 'Documentation'}
            {**
             Whether or not the light is enabled
            }
            {$ENDREGION}
            m_Enabled: Boolean;

        protected
            {$REGION 'Documentation'}
            {**
             Gets the ambient color
             @return(The ambient color)
            }
            {$ENDREGION}
            function GetAmbient: TQRColor; virtual;

            {$REGION 'Documentation'}
            {**
             Sets the ambient color
             @param(pColor The ambient color)
            }
            {$ENDREGION}
            procedure SetAmbient(const pColor: TQRColor); virtual;

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

            {$REGION 'Documentation'}
            {**
             Assigns (i.e. copies) the light content from another light
             @param(pOther Other light to assign from
            }
            {$ENDREGION}
            procedure Assign(const pOther: TQRLight); virtual;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the ambient color
            }
            {$ENDREGION}
            property Ambient: TQRColor read GetAmbient write SetAmbient;

            {$REGION 'Documentation'}
            {**
             Gets or sets whether the light is enabled
            }
            {$ENDREGION}
            property Enabled: Boolean read m_Enabled write m_Enabled;
    end;

    {$REGION 'Documentation'}
    {**
     Directional light
    }
    {$ENDREGION}
    TQRDirectionalLight = class(TQRLight)
        private
            {$REGION 'Documentation'}
            {**
             Light color
            }
            {$ENDREGION}
            m_pColor: TQRColor;

            {$REGION 'Documentation'}
            {**
             Light direction
            }
            {$ENDREGION}
            m_Direction: TQRVector3D;

        protected
            {$REGION 'Documentation'}
            {**
             Gets the light color
             @return(The light color)
            }
            {$ENDREGION}
            function GetColor: TQRColor; virtual;

            {$REGION 'Documentation'}
            {**
             Sets the light color
             @param(pColor The light color)
            }
            {$ENDREGION}
            procedure SetColor(const pColor: TQRColor); virtual;

            {$REGION 'Documentation'}
            {**
             Gets the light direction
             @return(The light direction)
            }
            {$ENDREGION}
            function GetDirection: PQRVector3D; virtual;

            {$REGION 'Documentation'}
            {**
             Sets the light direction
             @param(pDirection The light direction)
            }
            {$ENDREGION}
            procedure SetDirection(const pDirection: PQRVector3D); virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; override;

            {$REGION 'Documentation'}
            {**
            * Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Assigns (i.e. copies) the light content from another light
             @param(pOther Other light to assign from
            }
            {$ENDREGION}
            procedure Assign(const pOther: TQRLight); override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the light color
            }
            {$ENDREGION}
            property Color: TQRColor read GetColor write SetColor;

            {$REGION 'Documentation'}
            {**
             Gets or sets the light direction
            }
            {$ENDREGION}
            property Direction: PQRVector3D read GetDirection write SetDirection;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRLight
//--------------------------------------------------------------------------------------------------
constructor TQRLight.Create;
begin
    inherited Create;

    m_pAmbient := TQRColor.Create(0, 0, 0);
    m_Enabled  := False;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRLight.Destroy;
begin
    m_pAmbient.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRLight.GetAmbient: TQRColor;
begin
    Result := m_pAmbient;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRLight.SetAmbient(const pColor: TQRColor);
begin
    m_pAmbient.Assign(pColor);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRLight.Assign(const pOther: TQRLight);
begin
    // copy light from other
    m_pAmbient.Assign(pOther.m_pAmbient);
    m_Enabled := pOther.m_Enabled;
end;
//--------------------------------------------------------------------------------------------------
// TQRDirectionalLight
//--------------------------------------------------------------------------------------------------
constructor TQRDirectionalLight.Create;
begin
    inherited Create;

    m_pColor    := TQRColor.Create(0, 0, 0);
    m_Direction := TQRVector3D.Create(0.0, 0.0, 1.0);
end;
//--------------------------------------------------------------------------------------------------
destructor TQRDirectionalLight.Destroy;
begin
    m_pColor.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRDirectionalLight.GetColor: TQRColor;
begin
    Result := m_pColor;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRDirectionalLight.SetColor(const pColor: TQRColor);
begin
    m_pColor.Assign(pColor);
end;
//--------------------------------------------------------------------------------------------------
function TQRDirectionalLight.GetDirection: PQRVector3D;
begin
    Result := @m_Direction;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRDirectionalLight.SetDirection(const pDirection: PQRVector3D);
begin
    m_Direction := pDirection^;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRDirectionalLight.Assign(const pOther: TQRLight);
var
    pSource: TQRDirectionalLight;
begin
    inherited Assign(pOther);

    if (not(pOther is TQRDirectionalLight)) then
        Exit;

    pSource := pOther as TQRDirectionalLight;

    // copy light from other
    m_pColor.Assign(pSource.m_pColor);
    m_Direction := pSource.m_Direction;
end;
//--------------------------------------------------------------------------------------------------

end.
