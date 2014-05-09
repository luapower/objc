local ffi = require'ffi'
local objc = {debug = true}

-- Takes a type table (contains type info for a single type, obtained using parseTypeEncoding), and converts it to a C signature
-- The optional second argument specifies whether or not
local _typeEncodings = {
	["@"] = "id", ["#"] = "Class", ["c"] = "char", ["C"] = "unsigned char",
	["s"] = "short", ["S"] = "unsigned short", ["i"] = "int", ["I"] = "unsigned int",
	["l"] = "long", ["L"] = "unsigned long", ["q"] = "long long", ["Q"] = "unsigned long long",
	["f"] = "float", ["d"] = "double", ["B"] = "BOOL", ["v"] = "void", ["^"] = "void *", ["?"] = "void *",
	["*"] = "char *", [":"] = "SEL", ["?"] = "void", ["{"] = "struct", ["("] = "union", ["["] = "array"
}
function objc.typeToCType(type, varName)
	varName = varName or ""
	local ret = ""
	local encoding = type.type
	local ptrStr = ("*"):rep(type.indirection)

	-- Then type encodings
	local typeStr = _typeEncodings[encoding:sub(1,1)]

	if typeStr == nil then
		_log("Error! type encoding '", encoding, "' is not supported")
		return nil
	elseif typeStr == "union" then
		local unionType = _parseStructOrUnionEncoding(encoding, true)
		if unionType == nil then
			_log("Error! type encoding '", encoding, "' is not supported")
			return nil
		end
		ret = string.format("%s %s %s%s", ret, unionType, ptrStr, varName)
	elseif typeStr == "struct" then
		local structType = _parseStructOrUnionEncoding(encoding, false)
		--print(structType, encoding)
		if structType == nil then
			_log("Error! type encoding '", encoding, "' is not supported")
			return nil
		end
		ret = string.format("%s %s %s%s", ret, structType, ptrStr, varName)
	elseif typeStr == "array" then
		local arrType, arrCount = _parseArrayEncoding(encoding)
		if arrType == nil then
			_log("Error! type encoding '", encoding, "' is not supported")
			return nil
		end
		ret = string.format("%s %s%s[%d]", arrType, ptrStr, varName, arrCount)
	else
		ret = string.format("%s %s %s%s", ret, typeStr, ptrStr, varName)
	end

	return ret
end

-- Parses an ObjC type encoding string into an array of type dictionaries
function objc.parseTypeEncoding(str)
	local fieldIdx = 1
	local fields = { { name = "", type = "", indirection = 0, isConst = false} }
	local depth = 0
	local inQuotes = false
	local curField = fields[1]

	local temp, c
	for i=1, #str do
		c = str:sub(i,i)
		if     c:find("^[{%(%[]") then depth = depth + 1
		elseif c:find("^[}%)%]]") then depth = depth - 1
		elseif c == '"' then inQuotes = not inQuotes;
		end

		if depth > 0 then
			curField.type = curField.type .. c
		elseif inQuotes == true and c ~= '"' then
			curField.name = curField.name .. c
		elseif c == "^" then curField.indirection = curField.indirection + 1
		elseif c == "r" then curField.isConst = true
		elseif c:find('^["nobNRVr%^%d]') == nil then -- Skip over type qualifiers and bitfields
			curField.type = curField.type .. c
			fieldIdx = fieldIdx + 1
			fields[fieldIdx] = { name = "", type = "", indirection = 0 }
			curField = fields[fieldIdx]
		end
	end
	-- If the last field was blank, remove it
	if #fields[fieldIdx].name == 0 then
		table.remove(fields, fieldIdx)
	end
	return fields
end

-- Parses a struct/union encoding like {CGPoint="x"d"y"d}
local _definedStructs = setmetatable({}, { __mode = "kv" })
function _parseStructOrUnionEncoding(encoded, isUnion)
	local pat = "{([^=}]+)[=}]"
	local keyword = "struct"
	if isUnion == true then
		pat = '%(([^=%)]+)[=%)]'
		keyword = "union"
	end

	local unused, nameEnd, name = encoded:find(pat)
	local typeEnc = encoded:sub(nameEnd+1, #encoded-1)
	local fields = objc.parseTypeEncoding(typeEnc, '"')

	if name == "?" then name = "" end -- ? means an anonymous struct/union

	if     #fields <= 1 and name == "" then return keyword.."{} "..name
	elseif #fields <= 1 then return keyword.." "..name end

	local typeStr = _definedStructs[name]
	-- If the struct has been defined already, or does not have field name information, just return the name
	if typeStr ~= nil then
		print'here'
		return keyword.." "..name
	end

	typeStr = keyword.." "..name.." { "

	for i,f in pairs(fields) do
		local name = f.name
		if #name == 0 then name = "field"..tostring(i) end

		local type = objc.typeToCType(f, name)
		if type == nil then
			if objc.debug == true then _log("Unsupported type in ", keyword, name, ": ", f.type) end
			return nil
		end
		typeStr = typeStr .. type ..";"
	end
	typeStr = typeStr .." }"

	-- If the struct has a name we create a ctype and then just return the name for it. If it has none, we return the definition
	if #name > 0 then
		_definedStructs[name] = typeStr
		-- We need to wrap the def in a pcall so that we don't crash in case the struct is too big (As is the case with one in IOKit)
		local success, err = pcall(ffi.cdef, typeStr)
		if success == false then
			_log("Error loading struct ", name, ": ", err)
		end
		return keyword.." "..name
	else
		return typeStr
	end
end

print(_parseStructOrUnionEncoding'{CGRect="origin"{CGPoint="x"f"y"f}"size"{CGSize="width"f"height"f}}', false)

