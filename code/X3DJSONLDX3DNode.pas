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
  CastleImages,
  X3DNodes,
  X3DLoad;

type
  TX3DJSONLDX3DNode = class(TX3DFileItem)
  private
    x3dTidy: Boolean;
    
    function StripQuotes(const value: String): String;
    function CommentStringToXML(const str: String): String;
    function NavigationInfoTypeToXML(const str: String): String;
    
    procedure ElementSetAttribute(element: TX3DNode; const key: String; 
      const values: TStringList; rootNode: TX3DRootNode); overload;
    procedure ElementSetAttribute(element: TX3DNode; const key: String; 
      const value: String; rootNode: TX3DRootNode); overload;
    procedure SetField(element: TX3DNode; const key: String; const value: TPasJSONItem) overload;
    procedure SetField(element: TX3DNode; const key: String; const value: String) overload;
    
    function CreateElement(rootNode: TX3DRootNode; const key: String; 
      const containerField: String; obj: TPasJSONItemObject): TX3DNode;
    
    procedure CDATACreateFunction(rootNode: TX3DRootNode; element: TX3DNode; 
      const value: TPasJSONItemArray);
    
    procedure ConvertProperty(rootNode: TX3DRootNode; const key: String; 
      obj: TPasJSONItemObject; element: TX3DNode; const containerField: String);
    
    procedure ConvertJsonObject(rootNode: TX3DRootNode; obj: TPasJSONItemObject; 
      const parentkey: String; element: TX3DNode; const containerField: String);
    
    procedure ConvertJsonArray(rootNode: TX3DRootNode; arr: TPasJSONItemArray; 
      const parentkey: String; element: TX3DNode; const containerField: String);
    
    function ConvertJsonValue(rootNode: TX3DRootNode; value: TPasJSONItem; 
      const parentkey: String; element: TX3DNode; const containerField: String): TX3DNode;

    procedure AddChild(parentNode: TX3DNode; childNode: TAbstractChildNode); 
    function JSONArrayToImage(const valueArray: TPasJSONItemArray): TCastleImage;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    function LoadJsonIntoDocument(jsobj: TPasJSONItemObject; x3dTidyFlag: Boolean): TX3DRootNode;
    
    function ReadJsonFile(const filename: String): TPasJSONItem;
    function GetX3DVersion: TX3dVersion;
    function SerializeDOM(rootNode: TX3DRootNode): String;
  end;

implementation

uses
  X3DFields,
  SysUtils,
  Generics.Collections,
  StrUtils, XMLWrite,
  CastleLog;

constructor TX3DJSONLDX3DNode.Create;
begin
  inherited Create;
  x3dTidy := False;
end;

destructor TX3DJSONLDX3DNode.Destroy;
begin
  inherited Destroy;
end;

function TX3DJSONLDX3DNode.StripQuotes(const value: String): String;
begin
  if (Length(value) >= 2) and (value[1] = '"') and (value[Length(value)] = '"') then
    Result := Copy(value, 2, Length(value) - 2)
  else
    Result := value;
end;

procedure TX3DJSONLDX3DNode.ElementSetAttribute(element: TX3DNode; const key: String; 
  const values: TStringList; rootNode: TX3DRootNode);
var
  sb: String;
  i: Integer;
  fieldValue: TX3DNode;
  field: TX3DField;
begin
  sb := '';
  for i := 0 to values.Count - 1 do
  begin
    if i > 0 then
      sb := sb + ' ';
    sb := sb + values[i];
  end;
  
  if (element is TProtoInstance) and (key <> 'DEF') and (key <> 'name') then
  begin
    fieldValue := NodesManager.X3DTypeToClass('fieldValue', TX3DVersion).Create;
    SetField(element, 'name', key);
    SetField(element, 'value', sb);
    TProtoInstance(element).SetFieldValue(fieldValue);
  end
  else
    SetField(element,key, sb);
end;

procedure TX3DJSONLDX3DNode.SetField(element: TX3DNode; const key: String; const value: String) overload;
begin
    SetField(element, key, TPasJSONItemString(value));
end;

procedure TX3DJSONLDX3DNode.SetField(element: TX3DNode; const key: String; const value: TPasJSONItem) overload;
var
  field: TX3DField;
  valueArray: TPasJSONItemArray;
  NewMatrix3Value: TMatrix3;
begin
    field := element.Field(key);
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
    else
    if field is TMFInt32 then
	    TMFInt32(field).Value := value;
    else
    if field is TMFMatrix3d then
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
    else
    if field is TMFVec3d then
	    TMFVec3d(field).Value := value;
    else
    if field is TMFVec3f then
	    TMFVec3f(field).Value := value;
    else
    if field is TMFVec4d then
	    TMFVec4d(field).Value := value;
    else
    if field is TMFVec4f then
	    TMFVec4f(field).Value := value;
    else }
    if field is TSFBool then
    begin
	    TSFBool(field).Value := TPasJSON.getBoolean(value);
    end
    else if (field is TSFColor) and (value is TPasJSONItemArray) then
    begin
      valueArray := TPasJSONItemArray(value);
	    TSFColor(field).Value := Vector3(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]));
    end
    else if field is TSFColorRGBA then
    begin
      valueArray := TPasJSONItemArray(value);
	    TSFColorRGBA(field).Value := Vector4(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]),
        TPasJSON.getNumber(valueArray.Items[3]));
    end
    else if field is TSFDouble then
    begin
	    TSFDouble(field).Value := TPasJSON.getNumber(value);
    end
    else if field is TSFFloat then
    begin
	    TSFFloat(field).Value := TPasJSON.getNumber(value);
    end
    else if (field is TSFImage) and (key = 'image') then
    begin
      valueArray := TPasJSONItemArray(value);
	    TSFImage(field).Value := JSONArrayToImage(valueArray);
    end
    else if field is TSFInt32 then
    begin
	    TSFInt32(field).Value := TPasJSON.getInt64(value);
    end
    else if field is TSFMatrix3d then
    begin
      valueArray := TPasJSONItemArray(value);
      NewMatrix3Value := TMatrix3.Identity;
      NewMatrix3Value.Data[0][0] := TPasJSON.getNumber(valueArray.Items[0]);
      NewMatrix3Value.Data[1][0] := TPasJSON.getNumber(valueArray.Items[1]);
      NewMatrix3Value.Data[2][0] := TPasJSON.getNumber(valueArray.Items[2]);
      NewMatrix3Value.Data[0][1] := TPasJSON.getNumber(valueArray.Items[3]);
      NewMatrix3Value.Data[1][1] := TPasJSON.getNumber(valueArray.Items[4]);
      NewMatrix3Value.Data[2][1] := TPasJSON.getNumber(valueArray.Items[5]);
      NewMatrix3Value.Data[0][2] := TPasJSON.getNumber(valueArray.Items[6]);
      NewMatrix3Value.Data[1][2] := TPasJSON.getNumber(valueArray.Items[7]);
      NewMatrix3Value.Data[2][2] := TPasJSON.getNumber(valueArray.Items[8]);
      TSFMatrix3f(field).Value := NewMatrix3Value;
    end
    else if field is TSFMatrix3f then
    begin
      valueArray := TPasJSONItemArray(value);
      NewMatrix3Value := TMatrix3.Identity;
      NewMatrix3Value.Data[0][0] := TPasJSON.getNumber(valueArray.Items[0]);
      NewMatrix3Value.Data[1][0] := TPasJSON.getNumber(valueArray.Items[1]);
      NewMatrix3Value.Data[2][0] := TPasJSON.getNumber(valueArray.Items[2]);
      NewMatrix3Value.Data[0][1] := TPasJSON.getNumber(valueArray.Items[3]);
      NewMatrix3Value.Data[1][1] := TPasJSON.getNumber(valueArray.Items[4]);
      NewMatrix3Value.Data[2][1] := TPasJSON.getNumber(valueArray.Items[5]);
      NewMatrix3Value.Data[0][2] := TPasJSON.getNumber(valueArray.Items[6]);
      NewMatrix3Value.Data[1][2] := TPasJSON.getNumber(valueArray.Items[7]);
      NewMatrix3Value.Data[2][2] := TPasJSON.getNumber(valueArray.Items[8]);
      TSFMatrix3f(field).Value := NewMatrix3Value;
    end
    else if field is TSFMatrix4d then
    begin
	    TSFMatrix4d(field).Value :=  TMatrix4d.Create(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]),
        TPasJSON.getNumber(valueArray.Items[3]),
        TPasJSON.getNumber(valueArray.Items[4]),
        TPasJSON.getNumber(valueArray.Items[5]),
        TPasJSON.getNumber(valueArray.Items[6]),
        TPasJSON.getNumber(valueArray.Items[7]),
        TPasJSON.getNumber(valueArray.Items[8]),
        TPasJSON.getNumber(valueArray.Items[9]),
        TPasJSON.getNumber(valueArray.Items[10]),
        TPasJSON.getNumber(valueArray.Items[11]),
        TPasJSON.getNumber(valueArray.Items[12]),
        TPasJSON.getNumber(valueArray.Items[13]),
        TPasJSON.getNumber(valueArray.Items[14]),
        TPasJSON.getNumber(valueArray.Items[15]));
    end
    else if field is TSFMatrix4f then
    begin
	    TSFMatrix4f(field).Value :=  TMatrix4f.Create(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]),
        TPasJSON.getNumber(valueArray.Items[3]),
        TPasJSON.getNumber(valueArray.Items[4]),
        TPasJSON.getNumber(valueArray.Items[5]),
        TPasJSON.getNumber(valueArray.Items[6]),
        TPasJSON.getNumber(valueArray.Items[7]),
        TPasJSON.getNumber(valueArray.Items[8]),
        TPasJSON.getNumber(valueArray.Items[9]),
        TPasJSON.getNumber(valueArray.Items[10]),
        TPasJSON.getNumber(valueArray.Items[11]),
        TPasJSON.getNumber(valueArray.Items[12]),
        TPasJSON.getNumber(valueArray.Items[13]),
        TPasJSON.getNumber(valueArray.Items[14]),
        TPasJSON.getNumber(valueArray.Items[15]));
    { else if field is TSFNode then
      begin
	    TSFNode(field).Value := value;
      end }
    end
    else if field is TSFRotation then
    begin
	    TSFRotation(field).Value := Vector4(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]),
        TPasJSON.getNumber(valueArray.Items[3]));
    end
    else if field is TSFString then
    begin
	    TSFString(field).Value := TPasJSON.getString(value);
    end
    else if field is TSFTime then
    begin
	    TSFTime(field).Value := TPasJSON.getNumber(value);
    end
    else if field is TSFVec2d then
    begin
      valueArray := TPasJSONItemArray(value);
	    TSFVec2d(field).Value := Vector2Double(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]));
    end
    else if field is TSFVec2f then
    begin
      valueArray := TPasJSONItemArray(value);
	    TSFVec2f(field).Value := Vector2(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]));
    end
    else if field is TSFVec3d then
    begin
      valueArray := TPasJSONItemArray(value);
	    TSFVec3d(field).Value :=Vector3Double(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]));
    end
    else if field is TSFVec3f then
    begin
      valueArray := TPasJSONItemArray(value);
	    TSFVec3f(field).Value := Vector3(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]));
    end
    else if field is TSFVec4d then
    begin
      valueArray := TPasJSONItemArray(value);
	    TSFVec4d(field).Value := Vector4Double(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]),
        TPasJSON.getNumber(valueArray.Items[3]));
    end
    else if field is TSFVec4f then
    begin
      valueArray := TPasJSONItemArray(value);
	    TSFVec4f(field).Value := Vector4(
        TPasJSON.getNumber(valueArray.Items[0]),
        TPasJSON.getNumber(valueArray.Items[1]),
        TPasJSON.getNumber(valueArray.Items[2]),
        TPasJSON.getNumber(valueArray.Items[3]));
    end;
end;
// Add these cracker classes right after your "implementation" keyword
// and before your JSONArrayToImage function.
// Their only purpose is to expose the protected members of the image classes.
// The cracker classes are still needed.
type
  TCrackerGrayscaleImage = class(TGrayscaleImage);
  TCrackerGrayscaleAlphaImage = class(TGrayscaleAlphaImage);
  TCrackerRGBImage = class(TRGBImage);
  TCrackerRGBAlphaImage = class(TRGBAlphaImage);

// The final, working function.
function JSONArrayToImage(const AValueArray: TPasJSONItemArray): TCastleImage;
var
  Width, Height, Components, PixelCount, I: Integer;
  PixelValue: Cardinal;
  DestIndex: Integer;
  RawData: PByte; // A typed pointer to a Byte
begin
  Result := nil;

  // Metadata Parsing
  if AValueArray.Count < 3 then Exit;
  Width := TPasJSON.GetInt64(AValueArray.Items[0]);
  Height := TPasJSON.GetInt64(AValueArray.Items[1]);
  Components := TPasJSON.GetInt64(AValueArray.Items[2]);
  PixelCount := Width * Height;

  // Validation
  if AValueArray.Count <> (3 + PixelCount) then Exit;

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
      PixelValue := Cardinal(TPasJSON.GetInt64(AValueArray.Items[3 + I]));

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
      CastleLog.WritelnLog('Exception during image creation: ' + E.Message);
    end;
  end;
end;

procedure TX3DJSONLDX3DNode.ElementSetAttribute(element: TX3DNode; const key: String; 
  const value: String; rootNode: TX3DRootNode);
var
  fieldValue: TX3DNode;
begin
  if (element is  TProtoInstanceNode) and (key <> 'DEF') and (key <> 'name') then
  begin
    fieldValue := NodesManager.X3DTypeToClass('fieldValue', TX3DVersion).Create;
    SetField(fieldValue, 'name', key);
    SetField(fieldValue, 'value', value);
    TProtoInstanceNode(element).Add(fieldValue);
  end
  else if key = 'SON schema' then
  begin
    // JSON Schema - ignore
  end
  else if key = 'ncoding' then
  begin
    // encoding, UTF-8 - ignore
  end
  else
  begin
    SetField(fieldValue, key, value);
  end;
end;

function TX3DJSONLDX3DNode.CreateElement(rootNode: TX3DRootNode; const key: String; 
  const containerField: String; obj: TPasJSONItemObject): TX3DNode;
var
  child: TX3DNode;
  name, DEF: String;
begin
  child := NodesManager.X3DTypeToClass(key, TX3DVersion).Create;
  
  if (Length(containerField) > 0) and (containerField <> '') and (
    ((containerField = 'geometry') and (key = 'IndexedFaceSet')) or
    ((containerField = 'geometry') and (key = 'Text')) or
    ((containerField = 'geometry') and (key = 'IndexedTriangleSet')) or
    ((containerField = 'geometry') and (key = 'Sphere')) or
    ((containerField = 'geometry') and (key = 'Cylinder')) or
    ((containerField = 'geometry') and (key = 'Cone')) or
    ((containerField = 'geometry') and (key = 'LineSet')) or
    ((containerField = 'geometry') and (key = 'IndexedLineSet')) or
    ((containerField = 'geometry') and (key = 'Box')) or
    ((containerField = 'geometry') and (key = 'Extrusion')) or
    ((containerField = 'geometry') and (key = 'GeoElevationGrid')) or
    ((containerField = 'shape') and (key = 'Shape')) or
    ((containerField = 'skin') and (key = 'Shape')) or
    (EndsText('exture', containerField) and (key = 'ImageTexture')) or
    (key = 'HAnimSegment') or
    (key = 'HAnimSite') or
    (key = 'HAnimMotion') or
    ((containerField = 'skinCoord') and (key = 'Coordinate')) or
    ((containerField = 'skin') and (key = 'IndexedFaceSet')) or
    (((containerField = 'skinBindingCoords') or (containerField = 'skinCoord')) and (key = 'Coordinate')) or
    (((containerField = 'normal') or (containerField = 'skinBindingNormals') or (containerField = 'skinNormal')) and (key = 'Normal')) or
    (((containerField = 'skeleton') or (containerField = 'children') or (containerField = 'joints')) and (key = 'HAnimJoint'))
  ) then
  begin
    SetField(child, 'conntainerField', containerField);
  end;
  Result := child;
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
  
  { TODO element.CDATAField := sb; }
end;

procedure TX3DJSONLDX3DNode.ConvertProperty(rootNode: TX3DRootNode; const key: String; 
  obj: TPasJSONItemObject; element: TX3DNode; const containerField: String);
var
  jsonValue: TPasJSONItem;
  arr: TPasJSONItemArray;
  i: Integer;
begin
  if not Assigned(obj) then Exit;
  
  jsonValue := obj.Properties[key];
  if not Assigned(jsonValue) then Exit;
  
  if jsonValue is TPasJSONItemObject then
  begin
    if key = '@sourceCode' then
      CDATACreateFunction(rootNode, element, TPasJSONItemArray(jsonValue))
    else if (Length(key) > 0) and (key[1] = '@') then
      ConvertJsonValue(rootNode, jsonValue, key, element, containerField)
    else if (Length(key) > 0) and (key[1] = '-') then
      ConvertJsonValue(rootNode, jsonValue, key, element, Copy(key, 2, Length(key)-1))
    else if key = '#comment' then
    begin
      if jsonValue is TPasJSONItemArray then
      begin
        arr := TPasJSONItemArray(jsonValue);
        for i := 0 to arr.Count - 1 do
        begin
		{ comment := rootNode.CreateComment(CommentStringToXML(TPasJSON.getString(arr.Items[i])));
          element.Add(comment); }
        end;
      end
      else
      begin
	      { comment := rootNode.CreateComment(CommentStringToXML(TPasJSON.getString(jsonValue)));D
        element.Add(comment);j }
      end;
    end
    else if key = '#sourceCode' then
      CDATACreateFunction(rootNode, element, TPasJSONItemArray(jsonValue))
    else if (key = 'connect') or (key = 'fieldValue') or (key = 'field') or 
            (key = 'meta') or (key = 'component') or (key = 'unit') then
    begin
      arr := TPasJSONItemArray(jsonValue);
      ConvertJsonArray(rootNode, arr, key, element, containerField);
    end
    else
      ConvertJsonValue(rootNode, jsonValue, key, element, containerField);
  end;
end;

function TX3DJSONLDX3DNode.CommentStringToXML(const str: String): String;
var
  x, y: String;
begin
  y := str;
  Result := y;
  
  repeat
    x := Result;
    Result := StringReplace(x, '\"', '"', [rfReplaceAll]);
  until x = Result;
  
  repeat
    x := Result;
    Result := StringReplace(x, '""', '" "', [rfReplaceAll]);
  until x = Result;
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
  const parentkey: String; element: TX3DNode; const containerField: String);
var
  kii: Boolean;
  child: TX3DNode;
  i: Integer;
  key: String;
  jsonValue: TPasJSONItem;
  tempContainerField: String;
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
    tempContainerField := containerField;
    
    if ((tempContainerField = '') or (tempContainerField = 'children')) and 
       (parentkey = 'HAnimJoint') and (element.X3DType = 'HAnimHumanoid') then
      tempContainerField := 'joints';
      
    if ((tempContainerField = '') or (tempContainerField = 'coord')) and 
       (parentkey = 'Coordinate') and (element.X3DType = 'HAnimHumanoid') then
      tempContainerField := 'skinCoord';
      
    child := CreateElement(rootNode, parentkey, tempContainerField, obj);
  end;
  
  for i := 0 to obj.Count - 1 do
  begin
    key := obj.Keys[i];
    jsonValue := obj.Values[i];
    
    if jsonValue is TPasJSONItemObject then
    begin
      if (key = '@type') and (parentkey = 'NavigationInfo') then
        ElementSetAttribute(child, Copy(key, 2, Length(key)-1), 
          NavigationInfoTypeToXML(TPasJSON.getString(jsonValue)), rootNode)
      else if (Length(key) > 0) and (key[1] = '@') then
        ConvertProperty(rootNode, key, TPasJSONItemObject(jsonValue), child, containerField)
      else if (Length(key) > 0) and (key[1] = '-') then
        ConvertJsonObject(rootNode, TPasJSONItemObject(jsonValue), key, child, Copy(key, 2, Length(key)-1))
      else
        ConvertJsonObject(rootNode, TPasJSONItemObject(jsonValue), key, child, containerField);
    end
    else if jsonValue is TPasJSONItemArray then
      ConvertJsonArray(rootNode, TPasJSONItemArray(jsonValue), key, child, containerField)
    else if jsonValue is TPasJSONItemNumber then
      ElementSetAttribute(child, Copy(key, 2, Length(key)-1), TPasJSON.GetString(jsonValue), rootNode)
    else if jsonValue is TPasJSONItemString then
    begin
      if key = '#comment' then
      begin
	      { comment := rootNode.CreateComment(CommentStringToXML(TPasJSON.GetString(jsonValue)));
        child.Add(comment); }
      end
      else if (key = '@type') and (parentkey = 'NavigationInfo') then
        ElementSetAttribute(child, Copy(key, 2, Length(key)-1), 
          NavigationInfoTypeToXML(TPasJSON.GetString(jsonValue)), rootNode)
      else
        ElementSetAttribute(child, Copy(key, 2, Length(key)-1), TPasJSON.GetString(jsonValue), rootNode);
    end
    else if (jsonValue is TPasJSONItemBoolean) or (jsonValue is TPasJSONItemNull) then
      ElementSetAttribute(child, Copy(key, 2, Length(key)-1), TPasJSON.GetString(jsonValue), rootNode);
  end;
  
  if not kii and not ((Length(parentkey) > 0) and (parentkey[1] = '-')) then
  begin
    AddChild(element, TAbstractChildNode(child));
  end;
end;

procedure TX3DJSONLDX3DNode.AddChild(parentNode: TX3DNode; childNode: TAbstractChildNode); 
begin
  if parentNOde is TAbstractGroupingNode then
  begin
    TAbstractGroupingNode(parentNode).AddChildren(childNode);
  end
  else
  begin
    WriteLnLog('Warning: Tried to add a child to a non-grouping node of type %s', [parentNOde.ClassName]);
  end;
end;

procedure TX3DJSONLDX3DNode.ConvertJsonArray(rootNode: TX3DRootNode; arr: TPasJSONItemArray; 
  const parentkey: String; element: TX3DNode; const containerField: String);
var
  arrayOfStrings: Boolean;
  localArray: TStringList;
  arraysize, i: Integer;
  jsonValue: TPasJSONItem;
  kii: Boolean;
begin
  if not Assigned(arr) then Exit;
  
  arrayOfStrings := False;
  localArray := TStringList.Create;
  try
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
      
      if jsonValue is TPasJSONItemNumber then
        localArray.Add(TPasJSON.GetString(jsonValue))
      else if jsonValue is TPasJSONItemString then
      begin
        localArray.Add(TPasJSON.GetString(jsonValue));
        arrayOfStrings := True;
      end
      else if (jsonValue is TPasJSONItemBoolean) or (jsonValue is TPasJSONItemNull) then
        localArray.Add(TPasJSON.GetString(jsonValue))
      else if jsonValue is TPasJSONItemObject then
      begin
        try
          StrToInt(IntToStr(i));
          kii := True;
        except
          kii := False;
        end;
        
        if not ((Length(parentkey) > 0) and (parentkey[1] = '-')) and kii then
          ConvertJsonValue(rootNode, jsonValue, parentkey, element, containerField)
        else
          ConvertJsonValue(rootNode, jsonValue, IntToStr(i), element, Copy(parentkey, 2, Length(parentkey)-1));
      end
      else if jsonValue is TPasJSONItemArray then
        ConvertJsonValue(rootNode, jsonValue, IntToStr(i), element, containerField);
    end;
    
    if parentkey = '@sourceCode' then
      CDATACreateFunction(rootNode, element, arr)
    else if (Length(parentkey) > 0) and (parentkey[1] = '@') then
      ElementSetAttribute(element, Copy(parentkey, 2, Length(parentkey)-1), localArray, rootNode)
    else if parentkey = '#sourceCode' then
      CDATACreateFunction(rootNode, element, arr);
      
  finally
    localArray.Free;
  end;
end;

function TX3DJSONLDX3DNode.ConvertJsonValue(rootNode: TX3DRootNode; value: TPasJSONItem; 
  const parentkey: String; element: TX3DNode; const containerField: String): TX3DNode;
begin
  if value is TPasJSONItemArray then
    ConvertJsonArray(rootNode, TPasJSONItemArray(value), parentkey, element, containerField)
  else
    ConvertJsonObject(rootNode, TPasJSONItemObject(value), parentkey, element, containerField);
  
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
  
  element := CreateElement(Result, 'X3D', '', nil);
  ElementSetAttribute(element, 'xmlns:xsd', 'http://www.w3.org/2001/XMLSchema-instance', Result);
  
  x3dObj := TPasJSONItemObject(jsobj.Properties['X3D']);
  if Assigned(x3dObj) then
    ConvertJsonObject(Result, x3dObj, '-', element, '');
  
  AddChild(Result, TAbstractChildNode(element));
end;

function TX3DJSONLDX3DNode.ReadJsonFile(const filename: String): TPasJSONItem;
var
  fileStream: TFileStream;
  parser: TJSONParser;
begin
  fileStream := TFileStream.Create(filename, fmOpenRead);
  try
      Result := TPasJSON.Parse(fileStream);
  finally
    fileStream.Free;
  end;
end;

function TX3DJSONLDX3DNode.GetX3DVersion: TX3DVersion;
begin
  Result := TX3DVersion;
end;

function TX3DJSONLDX3DNode.SerializeDOM(rootNode: TX3DRootNode): String;
var
  stringStream: TStringStream;
  FileName: String;
begin
  stringStream := TStringStream.Create('');
  try
    FileName := 'outputScene.x3d';
    rootNode.SaveToXML;
    Result := stringStream.DataString;
  finally
    stringStream.Free;
  end;
end;

end.
