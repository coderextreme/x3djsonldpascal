{
Copyright (c) 2024. John Carlson
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
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
unit X3DJSONLD;

interface

uses CastlePasJSON, CastleXmlUtils, DOM;

type
  X3DJSON = class 
  private
    x3dTidy: boolean;
  public
    constructor Create();
    destructor Destroy; override;
    function  stripQuotes(value: String): String;
    procedure elementSetAttribute(element: TDOMElement; aKey: String; value: TPasJSONItemArray);
    procedure elementSetAttribute(element: TDOMElement; aKey: String; value: String);
    function  CreateElement(document: TDOMDocument; aKey: String; containerField: String): TDOMElement;
    procedure CDATACreateFunction(document: TDOMDocument; element: TDOMElement; value: TPasJSONItemArray);
    procedure convertProperty(document: TDOMDocument; aKey: String; itemObject: TPasJSONItemObject; element: TDOMElement; containerField: String);
    function  CommentStringToXML(str: String): String;
    function  NavigationInfoTypeToXML(str: String): String;
    function  fixXML(str: String; version: String): String;
    procedure convertJsonObject(document: TDOMDocument; itemObject: TPasJSONItemObject; parentKey: String; element: TDOMElement; containerField: String);
    procedure convertJsonArray(document: TDOMDocument; itemArray:  TPasJSONItemArray; parentKey: String; element: TDOMElement; containerField: String);
    function  convertJsonValue(document: TDOMDocument; itemObject: TPasJSONItem; parentKey: String; element: TDOMElement; containerField: String): TDOMElement;
    function  loadJsonIntoDocument(jsobj: TPasJSONItemObject; version: String; x3dTidyBoolean: boolean): TDOMDocument;
    { function  readJsonFile(jsonFile: File): TPasJSONItem; }
    function  getX3DVersion(jsobj: TPasJSONItemObject): String;
    procedure Load(url: String);
    function  serializeDOM(x3dVersion: String; document: TDOMDocument): String;
  end;

implementation

constructor X3DJSON.Create;
begin
  x3dTidy := false;
end;
destructor X3DJSON.Destroy;
begin
end;
function  X3DJSON.stripQuotes(value: String): String;
begin
	if ((value[1] == '"') and (value[Length(value)] == '"')) then
		result := Copy(value, 2, Length(value) - 2);
	else
		result := value;
end;
procedure X3DJSON.elementSetAttribute(element: TDOMElement; aKey: String; value: TPasJSONItemArray);
var jsonString: String;
begin
	for i := 0 to value.Count - 1 do
	begin
		jsonString := jsonString + value.Items[i].ToString;
	end;
	element.setAttribute(aKey, jsonString);
end;
procedure X3DJSON.elementSetAttribute(element: TDOMElement; aKey: String; value: String);
begin
	if (aKey == 'SON schema') then
		{ JSON Schema }
	else if (aKey == 'ncoding') then
		{ encoding, UTF-8 }
	else if (value == nil) then
		element.setAttribute(aKey, nil);
	else
		element.setAttribute(aKey, stripQuotes(value));
end;
function  X3DJSON.CreateElement(document: TDOMDocument; aKey: String; containerField: String): TDOMElement;
var child: TDOMElement;
begin
		child := document.createElement(aKey);
		if ((containerField != nil) and
				 ((containerField == 'geometry' ) and ((aKey == 'IndexedFaceSet') or
				 (aKey == 'Text') or
				 (aKey == 'IndexedTriangleSet') or
				 (aKey == 'Sphere') or
				 (aKey == 'Cylinder') or
				 (aKey == 'Cone') or
				 (aKey == 'LineSet') or
				 (aKey == 'IndexedLineSet') or
				 (aKey == 'Box') or
				 (aKey == 'Extrusion') or
				 (aKey == 'GeoElevationGrid'))) or
				 ((containerField == 'shape' ) and (aKey == 'Shape')) or
				 ((containerField == 'skin' ) and (aKey == 'Shape')) or
				 (EndsStr('exture', containerField) and (aKey == 'ImageTexture')) or
				 (aKey == 'HAnimSegment') or
				 (aKey == 'HAnimSite') or
				 (aKey == 'HAnimMotion') or
				 ((containerField == 'skinCoord' ) and (aKey == 'Coordinate')) or
				 ((containerField == 'skin' ) and (aKey == 'IndexedFaceSet')) or
				 (((containerField == 'skinBindingCoords') or (containerField == 'skinCoord')) and (aKey == 'Coordinate')) or
				 (((containerField == 'normal') or (containerField == 'skinBindingNormals') or (containerField == 'skinNormal')) and (aKey == 'Normal')) or
				 (((containerField == 'skeleton') or (containerField == 'children') or (containerField == 'joints')) and (aKey == 'HAnimJoint'))) then
			elementSetAttribute(child, 'containerField', containerField);
	result := child;
end;
procedure X3DJSON.CDATACreateFunction(document: TDOMDocument; element: TDOMElement; value: TPasJSONItemArray);
var strng: String;
begin
	for i := 0 to value.Count - 1 do
	begin
		strng := strng + value.Items[i].ToString;
	end;
	strng := strng.replaceAll('^"', '')
			.replaceAll('\\\\t', '\t')
			.replaceAll('"$', '')
			.replaceAll('&lt;', '<')
			.replaceAll('&gt;', '>')
			.replaceAll('&amp;', '&')
			.replaceAll('&quot;', '\"');
	TDOMCDATASection cdata := document.createCDATASection(strng);
	element.appendChild(cdata);
end;
procedure X3DJSON.convertProperty(document: TDOMDocument; aKey: String; itemObject: TPasJSONItemObject; element: TDOMElement; containerField: String);
begin
	if ((itemObject != nil) and (itemObject.get(aKey) instanceof TPasJSONItemObject)) then
		if (aKey == '@sourceCode') then
			CDATACreateFunction(document, element, (JsonArray)itemObject.get(aKey));
		else if (aKey[1] == '@') then
			convertJsonValue(document, itemObject.get(aKey), aKey, element, containerField);
		else if (aKey[1] == '-') then
			convertJsonValue(document, itemObject.get(aKey), aKey, element, aKey.substring(1));
		else if (aKey == '#comment') then
			if (itemObject.get(aKey) instanceof JsonArray) then
				JsonArray array := (JsonArray)itemObject.get(aKey);
				for (int childkey := 0; childkey <  array.size(); childkey++)
				begin
					Comment child := document.createComment(CommentStringToXML(array.get(childkey).toString()));
					element.appendChild(child);
				end;
			else
					Comment child := document.createComment(CommentStringToXML(itemObject.get(aKey).toString()));
					element.appendChild(child);
		else if (aKey == '#sourceCode') then
			CDATACreateFunction(document, element, (JsonArray)itemObject.get(aKey));
		else if (aKey == 'connect' or aKey == 'fieldValue' or aKey == 'field' or aKey == 'meta' or aKey == 'component' or aKey == 'unit') then
			JsonArray array := (JsonArray)itemObject.get(aKey);
			convertJsonArray(document, array, aKey, element, containerField);
		else
			convertJsonValue(document, itemObject.get(aKey), aKey, element, containerField);
end;
function  X3DJSON.CommentStringToXML(str: String): String;
begin
	result := '';
end;
function  X3DJSON.NavigationInfoTypeToXML(str: String): String;
begin
	result := '';
end;
function  X3DJSON.fixXML(str: String; version: String): String;
begin
	result := '';
end;
procedure X3DJSON.convertJsonObject(document: TDOMDocument; itemObject: TPasJSONItemObject; parentKey: String; element: TDOMElement; containerField: String);
begin
end;
procedure X3DJSON.convertJsonArray(document: TDOMDocument; itemArray:  TPasJSONItemArray; parentKey: String; element: TDOMElement; containerField: String);
begin
end;
function  X3DJSON.convertJsonValue(document: TDOMDocument; itemObject: TPasJSONItem; parentKey: String; element: TDOMElement; containerField: String): TDOMElement;
begin
	result := nil;
end;
function  X3DJSON.loadJsonIntoDocument(jsobj: TPasJSONItemObject; version: String; x3dTidyBoolean: boolean): TDOMDocument;
begin
	result := nil;
end;
{ class function  X3DJSON.readJsonFile(jsonFile: File): TPasJSONItem;
begin
	result := nil;
end; }
function  X3DJSON.getX3DVersion(jsobj: TPasJSONItemObject): String;
begin
	result := '';
end;
procedure X3DJSON.Load(url: String);
begin
end;
function  X3DJSON.serializeDOM(x3dVersion: String; document: TDOMDocument): String;
begin
	result := '';
end;
end.
