// *************************************************************************************************
// * ==> UTQRDesignPatterns -----------------------------------------------------------------------*
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
 @abstract(@name provides some classic ready-to-use design patterns classes)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRDesignPatterns;

interface

type
    {$REGION 'Documentation'}
    {**
     Generic message, e.g. sent from a subject to an observer
    }
    {$ENDREGION}
    TQRMessage = record
        {$REGION 'Documentation'}
        {**
         Generic type, meaning is left to discretion of the child classes implementation
        }
        {$ENDREGION}
        m_Type: NativeUInt;

        {$REGION 'Documentation'}
        {**
         Generic info, in case a subject should send additional specific info to his observer
        }
        {$ENDREGION}
        m_pInfo: Pointer
    end;

    {$REGION 'Documentation'}
    {**
     Generic observer
    }
    {$ENDREGION}
    IQRObserver = interface['{FA76EFFE-27CD-488D-B5E3-94CCE68AF113}']
        {$REGION 'Documentation'}
        {**
         Called when subject has sent a notification to the observer
         @param(message Notification message)
        }
        {$ENDREGION}
        procedure OnNotified(message: TQRMessage);
    end;

    {$REGION 'Documentation'}
    {**
     Generic subject
    }
    {$ENDREGION}
    IQRSubject = interface['{7541DE6F-48A4-42B6-90A2-18A2B24D2F07}']
        {$REGION 'Documentation'}
        {**
         Attaches observer
         @param(pObserver Observer to attach)
        }
        {$ENDREGION}
        procedure Attach(pObserver: IQRObserver);

        {$REGION 'Documentation'}
        {**
         Detaches observer
         @param(pObserver Observer to detach)
        }
        {$ENDREGION}
        procedure Detach(pObserver: IQRObserver);

        {$REGION 'Documentation'}
        {**
         Notifies all observers about an occurred event
         @param(message Notification message)
        }
        {$ENDREGION}
        procedure Notify(message: TQRMessage);
    end;

implementation

end.
