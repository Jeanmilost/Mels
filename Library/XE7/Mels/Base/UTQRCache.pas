{**************************************************************************************************
 * ==> UTQRCache ---------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides a ready-to-use cache system                                 *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRCache;

interface

uses System.Generics.Collections;

type
    {**
    * Called when new value is added to cache
    *@param key - newly added key
    *@param value - newly added avlue
    *}
    TQRCacheAddEvent<T, U> = procedure(const key: T; const value: U) of object;

    {**
    * Called when value is deleted from cache
    *@param[in, out] key - deleting key
    *@param[in, out] value - deleting value
    *@return true if value can be deleted from cache, otherwise false
    *}
    TQRCacheDeleteEvent<T, U> = function(const key: T; var value: U): Boolean of object;

    {**
    * Generic data caching class
    *@author Jean-Milost Reymond
    *}
    TQRCache<T, U> = class
        protected
            m_pCache:             TDictionary<T, U>;
            m_fOnAddToCache:      TQRCacheAddEvent<T, U>;
            m_fOnDeleteFromCache: TQRCacheDeleteEvent<T, U>;

            {**
            * Gets cached item count
            *@return cached item count
            *}
            function GetCount(): NativeUInt; virtual;

        public
            { Construction/Destruction }
            constructor Create();  virtual;
            destructor  Destroy(); override;

            {**
            * Clears cache
            *}
            procedure Clear(); virtual;

            {**
            * Adds value to cache
            *@param key - key
            *@param value - value to add
            *@return true on success, otherwise false
            *}
            function Add(const key: T; const value: U): Boolean; virtual;

            {**
            * Deletes value from cache
            *@param key - key
            *}
            procedure Delete(key: T); virtual;

            {**
            * Gets value from cache
            *@param key - key
            *@param[out] value - value to get
            *@return true if value exists, otherwise false
            *}
            function Get(const key: T; out value: U): Boolean; virtual;

            { Properties }
            property OnAddToCache:      TQRCacheAddEvent<T, U>    read m_fOnAddToCache      write m_fOnAddToCache;
            property OnDeleteFromCache: TQRCacheDeleteEvent<T, U> read m_fOnDeleteFromCache write m_fOnDeleteFromCache;
            property Count:             NativeUInt                read GetCount;
    end;

implementation
//------------------------------------------------------------------------------
// TQRCache
//------------------------------------------------------------------------------
constructor TQRCache<T, U>.Create();
begin
    inherited Create;

    m_pCache             := TDictionary<T, U>.Create;
    m_fOnAddToCache      := nil;
    m_fOnDeleteFromCache := nil;
end;
//------------------------------------------------------------------------------
destructor TQRCache<T, U>.Destroy();
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
//------------------------------------------------------------------------------
function TQRCache<T, U>.GetCount(): NativeUInt;
begin
    Result := m_pCache.Count;
end;
//------------------------------------------------------------------------------
procedure TQRCache<T, U>.Clear();
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
//------------------------------------------------------------------------------
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
        begin
            Result := False;
            Exit;
        end;

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
//------------------------------------------------------------------------------
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
//------------------------------------------------------------------------------
function TQRCache<T, U>.Get(const key: T; out value: U): Boolean;
begin
    // search for existing key in cache
    if (not m_pCache.ContainsKey(key)) then
    begin
        Result := False;
        Exit;
    end;

    // get item from cache
    value  := m_pCache.Items[key];
    Result := True;
end;
//------------------------------------------------------------------------------

end.
