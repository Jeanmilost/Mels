{**************************************************************************************************
 * ==> UTQRDesignPatterns ------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides some classic ready-to-use design patterns classes           *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRDesignPatterns;

interface

type
    {$REGION 'Observers'}

    {**
    * Generic message, e.g. sent from a subject to an observer
    *}
    TQRMessage = record
        public
            m_Type:  NativeUInt; // generic type, meaning is left to discretion of the child classes implementation
            m_pInfo: Pointer     // generic info, in case a subject should send additional specific info to his observer
    end;

    {**
    * Generic observer
    *}
    IQRObserver = interface['{FA76EFFE-27CD-488D-B5E3-94CCE68AF113}']
        {**
        * Called when subject has sent a notification to the observer
        *@param message - notification message
        *}
        procedure OnNotified(message: TQRMessage);
    end;

    {**
    * Generic subject
    *}
    IQRSubject = interface['{7541DE6F-48A4-42B6-90A2-18A2B24D2F07}']
        {**
        * Attaches observer
        *@param pObserver - observer to attach
        *}
        procedure Attach(pObserver: IQRObserver);

        {**
        * Detaches observer
        *@param pObserver - observer to detach
        *}
        procedure Detach(pObserver: IQRObserver);

        {**
        * Notifies all observers about an occurred event
        *@param message - notification message
        *}
        procedure Notify(message: TQRMessage);
    end;

    {$ENDREGION}

implementation

end.
