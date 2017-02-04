// *************************************************************************************************
// * ==> UTQRCache --------------------------------------------------------------------------------*
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
 @abstract(@name provides a ready-to-use cache system.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRCache;

{$MODE Delphi}

interface

uses Generics.Collections;

type
    {$REGION 'Documentation'}
    {**
     Called when new value is added to cache
     @param(key Newly added key)
     @param(value Newly added value)
    }
    {$ENDREGION}
    TQRCacheAddEvent<T, U> = procedure(const key: T; const value: U) of object;

    {$REGION 'Documentation'}
    {**
     Called when value is deleted from cache
     @param(key @bold([in, out]) Deleting key)
     @param(value @bold([in, out]) Deleting value)
     @return(@true if value can be deleted from cache, otherwise @false)
    }
    {$ENDREGION}
    TQRCacheDeleteEvent<T, U> = function(const key: T; var value: U): Boolean of object;

    {$REGION 'Documentation'}
    {**
     Generic data caching class
    }
    {$ENDREGION}
    TQRCache<T, U> = class
        private
            m_pCache:             TDictionary<T, U>;
            m_fOnAddToCache:      TQRCacheAddEvent<T, U>;
            m_fOnDeleteFromCache: TQRCacheDeleteEvent<T, U>;

        protected
            {$REGION 'Documentation'}
            {**
             Gets cached item count
             @return(Cached item count)
            }
            {$ENDREGION}
            function GetCount: NativeUInt; virtual;

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
             Clears cache
            }
            {$ENDREGION}
            procedure Clear; virtual;

            {$REGION 'Documentation'}
            {**
             Adds value to cache
             @param(key Key)
             @param(value Value to add)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Add(const key: T; const value: U): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Deletes value from cache
             @param(key Key)
            }
            {$ENDREGION}
            procedure Delete(key: T); virtual;

            {$REGION 'Documentation'}
            {**
             Gets value from cache
             @param(key Key)
             @param(value @bold([out]) Value to get)
             @return(@true if value exists, otherwise @false)
            }
            {$ENDREGION}
            function Get(const key: T; out value: U): Boolean; virtual;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the OnAddToCache event
            }
            {$ENDREGION}
            property OnAddToCache: TQRCacheAddEvent<T, U> read m_fOnAddToCache write m_fOnAddToCache;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnDeleteFromCache event
            }
            {$ENDREGION}
            property OnDeleteFromCache: TQRCacheDeleteEvent<T, U> read m_fOnDeleteFromCache write m_fOnDeleteFromCache;

            {$REGION 'Documentation'}
            {**
             Gets the cache item count
            }
            {$ENDREGION}
            property Count: NativeUInt read GetCount;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRCache
//--------------------------------------------------------------------------------------------------
constructor TQRCache<T, U>.Create;
begin
    inherited Create;

    m_pCache             := TDictionary<T, U>.Create;
    m_fOnAddToCache      := nil;
    m_fOnDeleteFromCache := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRCache<T, U>.Destroy;
var
    item:  TPair<T, U>;
    key:   T;
    value: U;
begin
    // iterate through all registered items
    for item in m_pCache do
    begin
        key   := item.Key;
        value := item.Value;

        // notify that item will be deleted
        if (Assigned(m_fOnDeleteFromCache)) then
            m_fOnDeleteFromCache(key, value);
    end;

    m_pCache.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRCache<T, U>.GetCount: NativeUInt;
begin
    Result := m_pCache.Count;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRCache<T, U>.Clear;
var
    item:  TPair<T, U>;
    key:   T;
    value: U;
begin
    // iterate through all registered items
    for item in m_pCache do
    begin
        key   := item.Key;
        value := item.Value;

        // notify that item will be deleted
        if (Assigned(m_fOnDeleteFromCache)) then
            m_fOnDeleteFromCache(key, value);
    end;

    m_pCache.Clear;
end;
//--------------------------------------------------------------------------------------------------
function TQRCache<T, U>.Add(const key: T; const value: U): Boolean;
var
    prevValue: U;
begin
    // search for existing key in cache
    if (m_pCache.ContainsKey(key)) then
    begin
        // get previous value to delete
        prevValue := m_pCache.Items[key];

        // notify that previous item is about to be deleted
        if (Assigned(m_fOnDeleteFromCache) and not m_fOnDeleteFromCache(key, prevValue)) then
            Exit(False);

        // delete previous item from cache
        m_pCache.Remove(key);
    end;

    // notify that item is about to be added
    if (Assigned(m_fOnAddToCache)) then
        m_fOnAddToCache(key, value);

    // add item to cache
    m_pCache.Add(key, value);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRCache<T, U>.Delete(key: T);
var
    prevValue: U;
begin
    // search for existing key in cache
    if (not m_pCache.ContainsKey(key)) then
        Exit;

    // get previous value to delete
    prevValue := m_pCache.Items[key];

    // notify that item is about to be deleted
    if (Assigned(m_fOnDeleteFromCache) and not m_fOnDeleteFromCache(key, prevValue)) then
        Exit;

    // delete item from cache
    m_pCache.Remove(key);
end;
//--------------------------------------------------------------------------------------------------
function TQRCache<T, U>.Get(const key: T; out value: U): Boolean;
begin
    // search for existing key in cache
    if (not m_pCache.ContainsKey(key)) then
        Exit(False);

    // get item from cache
    value  := m_pCache.Items[key];
    Result := True;
end;
//--------------------------------------------------------------------------------------------------

end.
