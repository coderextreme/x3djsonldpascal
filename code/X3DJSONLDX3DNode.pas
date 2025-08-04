unit X3DJSONLDX3DNode;

{
  Copyright (c) 2022. John Carlson
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the rootNodeation
    and/or other materials provided with the distribution.

  * Neither the name of content nor the names of its
    contributors may be used to endorse or promote products derived from
    this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE
}

interface

uses
  Classes,
  CastlePasJSON,
  { fpjson, }
  CastleImages,
  X3DNodes,
  X3DLoad;

type
  TX3DJSONLDX3DNode = class(TObject)
  { TX3DJSONLDX3DNode = class(TX3DFileItem) }
  private
    x3dTidy: Boolean;
    
    function NavigationInfoTypeToXML(const str: String): String;
    procedure ElementSetAttribute(element: TX3DNode; const key: String; const values: TStringList); overload;
    procedure ElementSetAttribute(element: TX3DNode; const key: String; const values: TPasJSONItemArray); overload;
    procedure ElementSetAttribute(element: TX3DNode; const key: String; const value: String); overload;
    procedure SetField(element: TX3DNode; const key: String; const value: TPasJSONItem) overload;
    procedure SetField(element: TX3DNode; const key: String; const value: String) overload;
    function CreateElement(const key: String): TX3DNode;
    procedure CDATACreateFunction(rootNode: TX3DRootNode; element: TX3DNode; const value: TPasJSONItemArray);
    procedure ConvertProperty(rootNode: TX3DRootNode; const key: String; obj: TPasJSONItemObject; element: TX3DNode);
    procedure ConvertJsonObject(rootNode: TX3DRootNode; obj: TPasJSONItemObject; const parentkey: String; element: TX3DNode);
    procedure ConvertJsonArray(rootNode: TX3DRootNode; arr: TPasJSONItemArray; const parentkey: String; element: TX3DNode);
    function ConvertJsonValue(rootNode: TX3DRootNode; value: TPasJSONItem; const parentkey: String; element: TX3DNode): TX3DNode;
    procedure AddChild(parentNode: TX3DNode; childNode: TX3DNode) overload; 
    function JSONArrayToImage(const valueArray: TPasJSONItemArray): TCastleImage;
    procedure CreateComment(element: TX3DNode; const arr: String);
    
  public
    class var localJSON: TX3DJSONLDX3DNode;
    constructor Create;
    destructor Destroy; override;
    procedure RegisterJSON;
    function LoadJsonIntoDocument(jsobj: TPasJSONItemObject; x3dTidyFlag: Boolean): TX3DRootNode;
  end;

implementation

uses
  X3DFields,
  SysUtils,
  Generics.Collections,
  StrUtils, XMLWrite,
  CastleVectors,
  CastleScene,
  CastleComponentSerialize,
  CastleLog;

type
  TCrackerGrayscaleImage = class(TGrayscaleImage);
  TCrackerGrayscaleAlphaImage = class(TGrayscaleAlphaImage);
  TCrackerRGBImage = class(TRGBImage);
  TCrackerRGBAlphaImage = class(TRGBAlphaImage);

constructor TX3DJSONLDX3DNode.Create;
begin
  inherited Create;
  x3dTidy := False;
  localJSON := Self;
end;

destructor TX3DJSONLDX3DNode.Destroy;
begin
  inherited Destroy;
end;

procedure TX3DJSONLDX3DNode.ElementSetAttribute(element: TX3DNode; const key: String; const values: TStringList) overload;
var
  sb: String;
  i: Integer;
  fieldValue: TX3DNode;
begin
  sb := '';
  for i := 0 to values.Count - 1 do
  begin
    if i > 0 then
      sb := sb + ' ';
    sb := sb + values[i];
  end;
  
  if (element is TX3DPrototypeNode) and (key <> 'DEF') and (key <> 'name') then
  begin
    fieldValue := NodesManager.X3DTypeToClass('fieldValue', X3DVersion).Create;
    SetField(fieldValue, 'name', key);
    SetField(fieldValue, 'value', sb);
    AddChild(element, TAbstractChildNode(fieldValue));
  end
  else
  begin
    SetField(element,key, sb);
  end;
end;

procedure TX3DJSONLDX3DNode.ElementSetAttribute(element: TX3DNode; const key: String; const values: TPasJSONItemArray); overload;
var
  i: Integer;
  sl: TSTringList;
begin
  sl := TStringList.Create;
  CastleLog.WriteLnLog('Array count '+IntToStr(values.Count));
  for i := 0 to values.Count - 2 do
  begin
    CastleLog.WriteLnLog('Array Index '+IntToStr(i));
    CastleLog.WriteLnLog('Array item '+TPasJSON.getString(values.Items[i]));
    sl.Add(TPasJSON.getString(values.Items[i]));
  end;
  ElementSetAttribute(element, key, sl);
end;

// The final, working function.
function TX3DJSONLDX3DNode.JSONArrayToImage(const valueArray: TPasJSONItemArray): TCastleImage;
var
  Width, Height, Components, PixelCount, I: Integer;
  PixelValue: Cardinal;
  DestIndex: Integer;
  RawData: PByte; // A typed pointer to a Byte
begin
  Result := nil;

  // Metadata Parsing
  if valueArray.Count < 3 then Exit;
  Width := TPasJSON.GetInt64(valueArray.Items[0]);
  Height := TPasJSON.GetInt64(valueArray.Items[1]);
  Components := TPasJSON.GetInt64(valueArray.Items[2]);
  PixelCount := Width * Height;

  // Validation
  if valueArray.Count <> (3 + PixelCount) then Exit;

  // Create the correct class instance
  case Components of
    1: Result := TGrayscaleImage.Create;
    2: Result := TGrayscaleAlphaImage.Create;
    3: Result := TRGBImage.Create;
    4: Result := TRGBAlphaImage.Create;
  else
    Exit;
  end;

  try
    // Set the image size, which allocates the internal FRawPixels buffer
    Result.SetSize(Width, Height);

    // --- THE KEY FIX ---
    // Get the protected pointer and cast it to a PByte *once* before the loop.
    RawData := PByte(TCrackerRGBImage(Result).FRawPixels);

    // Now we can use RawData with simple array notation.
    DestIndex := 0;
    for I := 0 to PixelCount - 1 do
    begin
      PixelValue := Cardinal(TPasJSON.GetInt64(valueArray.Items[3 + I]));

      case Components of
        4: // RGBA
        begin
          RawData[DestIndex]   := Byte((PixelValue shr 24) and $FF);
          RawData[DestIndex+1] := Byte((PixelValue shr 16) and $FF);
          RawData[DestIndex+2] := Byte((PixelValue shr 8) and $FF);
          RawData[DestIndex+3] := Byte(PixelValue and $FF);
        end;
        3: // RGB
        begin
          RawData[DestIndex]   := Byte((PixelValue shr 24) and $FF);
          RawData[DestIndex+1] := Byte((PixelValue shr 16) and $FF);
          RawData[DestIndex+2] := Byte((PixelValue shr 8) and $FF);
        end;
        2: // Grayscale + Alpha
        begin
          RawData[DestIndex]   := Byte((PixelValue shr 8) and $FF);
          RawData[DestIndex+1] := Byte(PixelValue and $FF);
        end;
        1: // Grayscale
        begin
          RawData[DestIndex]   := Byte(PixelValue and $FF);
        end;
      end;
      Inc(DestIndex, Components);
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      CastleLog.WriteLnLog('Exception during image creation: ' + E.Message);
    end;
  end;
end;

procedure TX3DJSONLDX3DNode.SetField(element: TX3DNode; const key: String; const value: String) overload;
begin
    SetField(element, key, TPasJSONItemString(value));
end;

procedure TX3DJSONLDX3DNode.SetField(element: TX3DNode; const key: String; const value: TPasJSONItem) overload;
var
  I: Integer;
  field: TX3DField;
  valueArray: TPasJSONItemArray;
  NewMatrix3dValue: TMatrix3Double;
  NewMatrix3fValue: TMatrix3;
  NewMatrix4dValue: TMatrix4Double;
  NewMatrix4fValue: TMatrix4;
  NewMFVec3dValue: array of TVector3;
  NewMFVec3fValue: array of TVector3;
  JSONData: Integer;
  JSONString: String;
begin
	{ CastleLog.WriteLnLog('DEBUG Setting '+element.ClassName+'.'+key+' to '+WriteJSON(value));
    CastleLog.WriteLnLog('DEBUG Setting '+element.ClassName+'.'+key+' to '+value.toJSON(true)); }
    if (element <> nil) and (key <> '') then begin
      field := element.Field(key);
    end else begin
      CastleLog.WriteLnLog('ERROR Can not find field ', key);
      Exit;
    end;
    {
    if field is TMFBool then
	    TMFBool(field).Value := value;
    else
    if field is TMFColor then
	    TMFColor(field).Value := value;
    else
    if field is TMFColorRGBA then
	    TMFColorRGBA(field).Value := value;
    else
    if field is TMFDouble then
	    TMFDouble(field).Value := value;
    else
    if field is TMFFloat then
	    TMFFloat(field).Value := value;
    else
    if field is TMFImage then
	    TMFImage(field).Value := value;
    else }
    if field is TMFInt32 then begin
      valueArray := TPasJSONItemArray(value);
      for I := 0 to valueArray.Count - 1 do begin
	CastleLog.WriteLnLog('MFInt32 '+IntToStr(I)+' '+TPasJSON.getString(valueArray.Items[I]));
      	TMFInt32(field).Items[I] := TPasJSON.getInt64(valueArray.Items[I]);
      end;
    end else if field is TMFVec3f then begin
      {
    end else if field is TMFMatrix3d then
	    TMFMatrix3d(field).Value := value;
    else
    if field is TMFMatrix3f then
	    TMFMatrix3f(field).Value := value;
    else
    if field is TMFMatrix4d then
	    TMFMatrix4d(field).Value := value;
    else
    if field is TMFMatrix4f then
	    TMFMatrix4f(field).Value := value;
    else
    if field is TMFNode then
	    TMFNode(field).Value := value;
    else
    if field is TMFRotation then
	    TMFRotation(field).Value := value;
    else
    if field is TMFString then
	    TMFString(field).Value := value;
    else
    if field is TMFTime then
	    TMFTime(field).Value := value;
    else
    if field is TMFVec2d then
	    TMFVec2d(field).Value := value;
    else
    if field is TMFVec2f then
	    TMFVec2f(field).Value := value;
    else }
    end else if field is TMFVec3d then begin
      valueArray := TPasJSONItemArray(value);
      for I := 0 to valueArray.Count div 3 do begin
      	TMFVec3d(field).Items[I] := Vector3Double(
	  TPasJSON.getNumber(valueArray.Items[I*3]),
	  TPasJSON.getNumber(valueArray.Items[I*3+1]),
	  TPasJSON.getNumber(valueArray.Items[I*3+2]));
      end;
    end else if field is TMFVec3f then begin
      valueArray := TPasJSONItemArray(value);
      for I := 0 to valueArray.Count div 3 do begin
      	TMFVec3f(field).Items[I] := Vector3(
	  TPasJSON.getNumber(valueArray.Items[I*3]),
	  TPasJSON.getNumber(valueArray.Items[I*3+1]),
	  TPasJSON.getNumber(valueArray.Items[I*3+2]));
      end;
	{ else if field is TMFVec4d then begin
	    TMFVec4d(field).Value := value;
    end else if field is TMFVec4f then begin
	    TMFVec4f(field).Value := value; }
    end else if field is TSFBool then begin
       TSFBool(field).Value := TPasJSON.getBoolean(value);
    end else if (field is TSFColor) and (value is TPasJSONItemArray) then begin
      valueArray := TPasJSONItemArray(value);
      TSFColor(field).Value := Vector3(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]));
    end else if (field is TSFColorRGBA) and (value is TPasJSONItemArray) then begin
      valueArray := TPasJSONItemArray(value);
      TSFColorRGBA(field).Value := Vector4(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]),
        TPasJSON.getNumber(valueArray.Items[3]));
    end else if (field is TSFDouble) then begin
      TSFDouble(field).Value := TPasJSON.getNumber(value);
    end else if (field is TSFFloat) then begin
      TSFFloat(field).Value := TPasJSON.getNumber(value);
    end else if (field is TSFImage) and (key = 'image') and (value is TPasJSONItemArray) then begin
      valueArray := TPasJSONItemArray(value);
      TSFImage(field).Value := JSONArrayToImage(valueArray);
    end else if (field is TSFImage) and (key = 'url') and (value is TPasJSONItemArray) then begin
      valueArray := TPasJSONItemArray(value);
      raise Exception.Create('Image URLs not handled');
    end else if (field is TSFInt32) then begin
      TSFInt32(field).Value := TPasJSON.getInt64(value);
    end else if (field is TSFMatrix3d) and (value is TPasJSONItemArray) then begin
      valueArray := TPasJSONItemArray(value);
      if valueArray.Count < 9 then
        raise Exception.Create('Matrix array must have 9 elements');
      NewMatrix3dValue := TMatrix3Double.Identity;
      NewMatrix3dValue.Data[0][0] := TPasJSON.getNumber(valueArray.Items[0]);
      NewMatrix3dValue.Data[1][0] := TPasJSON.getNumber(valueArray.Items[1]);
      NewMatrix3dValue.Data[2][0] := TPasJSON.getNumber(valueArray.Items[2]);
      NewMatrix3dValue.Data[0][1] := TPasJSON.getNumber(valueArray.Items[3]);
      NewMatrix3dValue.Data[1][1] := TPasJSON.getNumber(valueArray.Items[4]);
      NewMatrix3dValue.Data[2][1] := TPasJSON.getNumber(valueArray.Items[5]);
      NewMatrix3dValue.Data[0][2] := TPasJSON.getNumber(valueArray.Items[6]);
      NewMatrix3dValue.Data[1][2] := TPasJSON.getNumber(valueArray.Items[7]);
      NewMatrix3dValue.Data[2][2] := TPasJSON.getNumber(valueArray.Items[8]);
      TSFMatrix3d(field).Value := NewMatrix3dValue;
    end else if (field is TSFMatrix3f) and (value is TPasJSONItemArray) then begin
      valueArray := TPasJSONItemArray(value);
      if valueArray.Count < 9 then
        raise Exception.Create('Matrix array must have 9 elements');
      NewMatrix3fValue := TMatrix3.Identity;
      NewMatrix3fValue.Data[0][0] := TPasJSON.getNumber(valueArray.Items[0]);
      NewMatrix3fValue.Data[1][0] := TPasJSON.getNumber(valueArray.Items[1]);
      NewMatrix3fValue.Data[2][0] := TPasJSON.getNumber(valueArray.Items[2]);
      NewMatrix3fValue.Data[0][1] := TPasJSON.getNumber(valueArray.Items[3]);
      NewMatrix3fValue.Data[1][1] := TPasJSON.getNumber(valueArray.Items[4]);
      NewMatrix3fValue.Data[2][1] := TPasJSON.getNumber(valueArray.Items[5]);
      NewMatrix3fValue.Data[0][2] := TPasJSON.getNumber(valueArray.Items[6]);
      NewMatrix3fValue.Data[1][2] := TPasJSON.getNumber(valueArray.Items[7]);
      NewMatrix3fValue.Data[2][2] := TPasJSON.getNumber(valueArray.Items[8]);
      TSFMatrix3f(field).Value := NewMatrix3fValue;
    end else if (field is TSFMatrix4d) and (value is TPasJSONItemArray) then begin
      valueArray := TPasJSONItemArray(value);
      if valueArray.Count < 16 then
        raise Exception.Create('Matrix array must have 16 elements');
  
      {NewMatrix4dValue: TMatrix4;}
      NewMatrix4dValue := TMatrix4Double.Identity;
      NewMatrix4dValue.Data[0][0] := TPasJSON.getNumber(valueArray.Items[0]);
      NewMatrix4dValue.Data[1][0] := TPasJSON.getNumber(valueArray.Items[1]);
      NewMatrix4dValue.Data[2][0] := TPasJSON.getNumber(valueArray.Items[2]);
      NewMatrix4dValue.Data[3][0] := TPasJSON.getNumber(valueArray.Items[3]);
      NewMatrix4dValue.Data[0][1] := TPasJSON.getNumber(valueArray.Items[4]);
      NewMatrix4dValue.Data[1][1] := TPasJSON.getNumber(valueArray.Items[5]);
      NewMatrix4dValue.Data[2][1] := TPasJSON.getNumber(valueArray.Items[6]);
      NewMatrix4dValue.Data[3][1] := TPasJSON.getNumber(valueArray.Items[7]);
      NewMatrix4dValue.Data[0][2] := TPasJSON.getNumber(valueArray.Items[8]);
      NewMatrix4dValue.Data[1][2] := TPasJSON.getNumber(valueArray.Items[9]);
      NewMatrix4dValue.Data[2][2] := TPasJSON.getNumber(valueArray.Items[10]);
      NewMatrix4dValue.Data[3][2] := TPasJSON.getNumber(valueArray.Items[11]);
      NewMatrix4dValue.Data[0][3] := TPasJSON.getNumber(valueArray.Items[12]);
      NewMatrix4dValue.Data[1][3] := TPasJSON.getNumber(valueArray.Items[13]);
      NewMatrix4dValue.Data[2][3] := TPasJSON.getNumber(valueArray.Items[14]);
      NewMatrix4dValue.Data[3][3] := TPasJSON.getNumber(valueArray.Items[15]);
      TSFMatrix4d(field).Value := NewMatrix4dValue;
    end else if (field is TSFMatrix4f) and (value is TPasJSONItemArray) then begin
      valueArray := TPasJSONItemArray(value);
      if valueArray.Count < 16 then
        raise Exception.Create('Matrix array must have 16 elements');
      NewMatrix4fValue := TMatrix4.Identity;
      NewMatrix4fValue.Data[0][0] := TPasJSON.getNumber(valueArray.Items[0]);
      NewMatrix4fValue.Data[1][0] := TPasJSON.getNumber(valueArray.Items[1]);
      NewMatrix4fValue.Data[2][0] := TPasJSON.getNumber(valueArray.Items[2]);
      NewMatrix4fValue.Data[3][0] := TPasJSON.getNumber(valueArray.Items[3]);
      NewMatrix4fValue.Data[0][1] := TPasJSON.getNumber(valueArray.Items[4]);
      NewMatrix4fValue.Data[1][1] := TPasJSON.getNumber(valueArray.Items[5]);
      NewMatrix4fValue.Data[2][1] := TPasJSON.getNumber(valueArray.Items[6]);
      NewMatrix4fValue.Data[3][1] := TPasJSON.getNumber(valueArray.Items[7]);
      NewMatrix4fValue.Data[0][2] := TPasJSON.getNumber(valueArray.Items[8]);
      NewMatrix4fValue.Data[1][2] := TPasJSON.getNumber(valueArray.Items[9]);
      NewMatrix4fValue.Data[2][2] := TPasJSON.getNumber(valueArray.Items[10]);
      NewMatrix4fValue.Data[3][2] := TPasJSON.getNumber(valueArray.Items[11]);
      NewMatrix4fValue.Data[0][3] := TPasJSON.getNumber(valueArray.Items[12]);
      NewMatrix4fValue.Data[1][3] := TPasJSON.getNumber(valueArray.Items[13]);
      NewMatrix4fValue.Data[2][3] := TPasJSON.getNumber(valueArray.Items[14]);
      NewMatrix4fValue.Data[3][3] := TPasJSON.getNumber(valueArray.Items[15]);
      TSFMatrix4f(field).Value := NewMatrix4fValue;
    { else if field is TSFNode then
      begin
	    TSFNode(field).Value := value;
      end }
    end else if (field is TSFRotation) and (value is TPasJSONItemArray) then begin
      TSFRotation(field).Value := Vector4(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]),
        TPasJSON.getNumber(valueArray.Items[3]));
    end
    else if (field is TSFString) then begin TSFString(field).Value := TPasJSON.getString(value);
    end
    else if (field is TSFTime) then begin TSFTime(field).Value := TPasJSON.getNumber(value);
    end else if (field is TSFVec2d) and (value is TPasJSONItemArray) then begin
      valueArray := TPasJSONItemArray(value);
      TSFVec2d(field).Value := Vector2Double(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]));
    end else if (field is TSFVec2f) and (value is TPasJSONItemArray) then begin
      valueArray := TPasJSONItemArray(value);
	    TSFVec2f(field).Value := Vector2(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]));
    end else if (field is TSFVec3d) and (value is TPasJSONItemArray) then begin
      valueArray := TPasJSONItemArray(value);
      TSFVec3d(field).Value :=Vector3Double(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]));
    end else if (field is TSFVec3f) and (value is TPasJSONItemArray) then begin
      valueArray := TPasJSONItemArray(value);
      TSFVec3f(field).Value := Vector3(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]));
    end else if (field is TSFVec4d) and (value is TPasJSONItemArray) then begin
      valueArray := TPasJSONItemArray(value);
      TSFVec4d(field).Value := Vector4Double(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]),
        TPasJSON.getNumber(valueArray.Items[3]));
    end else if (field is TSFVec4f) and (value is TPasJSONItemArray) then begin
      valueArray := TPasJSONItemArray(value);
      TSFVec4f(field).Value := Vector4(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]),
        TPasJSON.getNumber(valueArray.Items[3]));
    end;
end;

procedure TX3DJSONLDX3DNode.ElementSetAttribute(element: TX3DNode; const key: String; const value: String) overload;
var
  fieldValue: TX3DNode;
  keyCopy: String;
begin
  keyCopy := key;
  if (Length(keyCopy) > 0) and ((keyCopy[1] = '-') or (keyCopy[1] = '@')) then
  begin
    keyCopy := Copy(keyCopy, 2, Length(keyCopy)-1);
    CastleLog.writeLnLog('key is', keyCopy);
  end;
  if (element is  TX3DPrototypeNode) and (keyCopy <> 'DEF') and (keyCopy <> 'name') then
  begin
    fieldValue := NodesManager.X3DTypeToClass('fieldValue', X3DVersion).Create;
    SetField(fieldValue, 'name', keyCopy);
    SetField(fieldValue, 'value', value);
    AddChild(element, TAbstractChildNode(fieldValue));
  end
  else if keyCopy = 'JSON schema' then
  begin
    // JSON Schema - ignore
  end
  else if keyCopy = 'encoding' then
  begin
    // encoding, UTF-8 - ignore
  end
  else
  begin
    SetField(element, keyCopy, value);
  end;
end;

function TX3DJSONLDX3DNode.CreateElement(const key: String): TX3DNode;
var
  clz: TX3DNodeClass;
begin
  CastleLog.WriteLnLog('DEBUG', 'Creating Element for '+key);
  clz := NodesManager.X3DTypeToClass(key, X3DVersion);
  if Assigned(clz) then begin
    Result := clz.Create;
  end else begin
    CastleLog.WriteLnLog('ERROR', 'Could not find Element for');
    Result := nil;
  end;
end;

procedure TX3DJSONLDX3DNode.CDATACreateFunction(rootNode: TX3DRootNode; element: TX3DNode; 
  const value: TPasJSONItemArray);
var
  sb: String;
  i: Integer;
begin
  sb := '';
  for i := 0 to value.Count - 1 do
  begin
    if i > 0 then
      sb := sb + #10;
    sb := sb + TPasJSON.getString(value.Items[i]);
  end;
  
  { element.CDATAField.Value := sb; }
end;

procedure TX3DJSONLDX3DNode.CreateComment(element: TX3DNode; const arr: String);
begin
  { TODO  I don'g tknow how to implement this! }
  { AddChild(element, TX3DCommentNode.Create(str)); }
end;

procedure TX3DJSONLDX3DNode.ConvertProperty(rootNode: TX3DRootNode; const key: String; 
  obj: TPasJSONItemObject; element: TX3DNode);
var
  jsonValue: TPasJSONItem;
  arr: TPasJSONItemArray;
  i: Integer;
begin
  if not Assigned(obj) then Exit;
  
  jsonValue := obj.Properties[key];
  if not Assigned(jsonValue) then Exit;
  
  if jsonValue is TPasJSONItemObject then begin
    if (key = '@sourceCode') or (key = '#sourceCode') then begin
      CDATACreateFunction(rootNode, element, TPasJSONItemArray(jsonValue))
    end else if (Length(key) > 0) and (key[1] = '@') then begin
      ConvertJsonValue(rootNode, jsonValue, key, element)
    end else if (Length(key) > 0) and (key[1] = '-') then begin
      ConvertJsonValue(rootNode, jsonValue, key, element)
    end else if key = '#comment' then begin
      if jsonValue is TPasJSONItemArray then begin
        arr := TPasJSONItemArray(jsonValue);
        for i := 0 to arr.Count - 1 do begin
          CreateComment(element, TPasJSON.getString(arr.Items[i]));
        end;
      end else begin
        CreateComment(element, TPasJSON.getString(jsonValue));
      end;
    end else if (key = 'connect') or (key = 'fieldValue') or (key = 'field') or 
            (key = 'meta') or (key = 'component') or (key = 'unit') then begin
      arr := TPasJSONItemArray(jsonValue);
      ConvertJsonArray(rootNode, arr, key, element);
    end else begin
      ConvertJsonValue(rootNode, jsonValue, key, element);
    end;
  end;
end;

function TX3DJSONLDX3DNode.NavigationInfoTypeToXML(const str: String): String;
begin
  WriteLn(StdErr, 'TX3DJSONLDX3DNode jsonstring replacing ', str);
  Result := StringReplace(str, '\', '', [rfReplaceAll]);
  if str <> Result then
    WriteLn(StdErr, 'with                           ', Result)
  else
    WriteLn(StdErr, 'ok');
end;

procedure TX3DJSONLDX3DNode.ConvertJsonObject(rootNode: TX3DRootNode; obj: TPasJSONItemObject; 
  const parentkey: String; element: TX3DNode);
var
  kii: Boolean;
  child: TX3DNode;
  i: Integer;
  key: String;
  jsonValue: TPasJSONItem;
begin
  if not Assigned(obj) then Exit;
  
  // Check if parentkey is numeric
  try
    StrToInt(parentkey);
    kii := True;
  except
    kii := False;
  end;
  
  if kii or (Length(parentkey) > 0) and (parentkey[1] = '-') then
    child := element
  else
  begin
    child := CreateElement(parentkey);
  end;
  
  for i := 0 to obj.Count - 1 do
  begin
    key := obj.Keys[i];
    jsonValue := obj.Values[i];
    
    if jsonValue is TPasJSONItemObject then
    begin
      if (key = '@type') and (parentkey = 'NavigationInfo') then begin
        ElementSetAttribute(child, key, NavigationInfoTypeToXML(TPasJSON.getString(jsonValue)))
      end else if (Length(key) > 0) and (key[1] = '@') then begin
        ConvertProperty(rootNode, key, TPasJSONItemObject(jsonValue), child)
      end else if (Length(key) > 0) and (key[1] = '-') then begin
        ConvertJsonObject(rootNode, TPasJSONItemObject(jsonValue), key, child)
      end else begin
        ConvertJsonObject(rootNode, TPasJSONItemObject(jsonValue), key, child);
      end;
    end else if jsonValue is TPasJSONItemArray then begin
      ConvertJsonArray(rootNode, TPasJSONItemArray(jsonValue), key, child)
    end else if jsonValue is TPasJSONItemNumber then begin
      ElementSetAttribute(child, key, TPasJSON.GetString(jsonValue))
    end else if jsonValue is TPasJSONItemString then begin
      if key = '#comment' then begin
        CreateComment(child, TPasJSON.GetString(jsonValue));
      end else if (key = '@type') and (parentkey = 'NavigationInfo') then begin
        ElementSetAttribute(child, key, NavigationInfoTypeToXML(TPasJSON.GetString(jsonValue)));
      end else begin
        ElementSetAttribute(child, key, TPasJSON.GetString(jsonValue));
      end;
    end else if (jsonValue is TPasJSONItemBoolean) or (jsonValue is TPasJSONItemNull) then begin
      ElementSetAttribute(child, key, TPasJSON.GetString(jsonValue));
    end;
  end;
  
  if not kii and not ((Length(parentkey) > 0) and (parentkey[1] = '-')) then begin
    if (child is TAbstractChildNode) then begin
      AddChild(element, TAbstractChildNode(child));
    end else begin
      CastleLog.WriteLnLog('ERROR cannot cast');
    end;
  end;
end;

procedure TX3DJSONLDX3DNode.AddChild(parentNode: TX3DNode; childNode: TX3DNode); 
var
  AShape: TShapeNode;
  AGeometry: TAbstractGeometryNode;
begin
  if (parentNode is TAbstractGroupingNode) and (childNode is TAbstractChildNode) then begin
    TAbstractGroupingNode(parentNode).AddChildren(TAbstractChildNode(childNode));
  end else if (parentNode is TX3dRootNode) and (childNode is TAbstractChildNode) then begin
    TX3DRootNode(parentNode).AddChildren(TAbstractChildNode(childNode));
  end else if (parentNode is TX3DRootNode) and (childNode is TAbstractGeometryNode) then begin
    AShape := TShapeNode.Create;
    TAbstractGeometryNode(childNode).CreateWithShape(AShape);
    TX3DRootNode(parentNode).AddChildren(AShape);
  end else begin
    WriteLnLog('Warning: Tried to add a child node of type %s to a parent node of type %s', [childNode.ClassName, parentNode.ClassName]);
  end;
end;

procedure TX3DJSONLDX3DNode.ConvertJsonArray(rootNode: TX3DRootNode; arr: TPasJSONItemArray; 
  const parentkey: String; element: TX3DNode);
var
  arraysize, i: Integer;
  jsonValue: TPasJSONItem;
begin
  if not Assigned(arr) then Exit;
  arraysize := arr.Count;
  
  if parentkey = 'meta' then
  begin
    if (x3dTidy) then
    begin
      arraysize := arr.Count - 2;
    end
    else
    begin
      arraysize := arr.Count - 3;
    end
  end;
  
  for i := 0 to arraysize - 1 do
  begin
    jsonValue := arr.Items[i];
    ConvertJsonValue(rootNode, jsonValue, parentkey, element)
  end;
  
  if (parentkey = '@sourceCode') or (parentkey = '#sourceCode') then
  begin
    CDATACreateFunction(rootNode, element, arr);
  end
  else if (Length(parentkey) > 0) and (parentkey[1] = '@') then
  begin
    ElementSetAttribute(element, parentkey, arr);
  end;
end;

function TX3DJSONLDX3DNode.ConvertJsonValue(rootNode: TX3DRootNode; value: TPasJSONItem; 
  const parentkey: String; element: TX3DNode): TX3DNode;
begin
  if value is TPasJSONItemArray then begin
    ConvertJsonArray(rootNode, TPasJSONItemArray(value), parentkey, element)
  end else if value is TPasJSONItemObject then begin
    ConvertJsonObject(rootNode, TPasJSONItemObject(value), parentkey, element);
  end else begin
      CastleLog.WriteLnLog('Could not do children of : '+ parentkey + ' value is '+TPasJSON.GetString(value));
  end;
  
  Result := element;
end;

function TX3DJSONLDX3DNode.LoadJsonIntoDocument(jsobj: TPasJSONItemObject;
  x3dTidyFlag: Boolean): TX3DRootNode;
var
  element: TX3DNode;
  x3dObj: TPasJSONItemObject;
begin
  x3dTidy := x3dTidyFlag;
  
  Result := TX3DRootNode.Create;
  
  element := CreateElement('X3D');
  ElementSetAttribute(element, 'xmlns:xsd', 'http://www.w3.org/2001/XMLSchema-instance');
  
  x3dObj := TPasJSONItemObject(jsobj.Properties['X3D']);
  if Assigned(x3dObj) then
    ConvertJsonObject(Result, x3dObj, '-', element);
  
  AddChild(Result, element);
end;

function LoadX3DJsonInternal(const Stream: TStream; const BaseUrl: String): TX3DRootNode; overload;
var
    jsobj: TPasJSONItem;
begin
  try
  { fileStream := TFileStream.Create(filename, fmOpenRead); }
    jsobj := TPasJSON.Parse(Stream);
    if jsobj is TPasJsonItemObject then
    begin
      Result := TX3DJSONLDX3DNode.localJSON.LoadJsonIntoDocument(TPasJsonItemObject(jsobj), False);
    end;
  finally
    jsobj.Free;
  end;
end;

procedure TX3DJSONLDX3DNode.RegisterJSON();
var
  ModelFormat: TModelFormat;
begin
  ModelFormat := TModelFormat.Create;
  ModelFormat.OnLoad := {$ifdef FPC}@{$endif} LoadX3DJsonInternal;
  ModelFormat.MimeTypes.Add('model/x3d+json');
  ModelFormat.FileFilterName := 'X3D JSON (*.x3dj)';
  ModelFormat.Extensions.Add('.x3dj');
  RegisterModelFormat(ModelFormat);
end;

begin
end.
