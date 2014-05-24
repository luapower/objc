local objc = require'objc'

local inspect = {}

--pretty helpers

local function p(...) --formatted line
	print(_(...))
end

local function hr() --horizontal line
	print(('-'):rep(100))
end

local function header(...) --formatted header
	print''
	hr'+'
	p('| '.._(...))
	hr'+'
end

local comp = memoize(function(getname)
	return function(a, b)
		return getname(a) < getname(b)
	end
end)
local function sorta(a, getname) --sort a C array by name
	local t = {}
	for i,e in apairs(a) do
		t[#t+1] = e
	end
	table.sort(t, comp(getname or tostring))
	return t
end

--pretty chunks

local function inspect_properties(props)
	if not props then return end
	p('\n%-40s %-20s %-40s %-40s', 'Properties:', 'ctype', 'getter', 'setter')
	hr()
	for i,prop in ipairs(sorta(props)) do
		p('%-40s %-20s %-40s %-40s', property_name(prop),
			type_ctype(property_ctype(prop)),
			property_getter(prop),
			property_setter(prop) or 'n/a')
	end
end

local function inspect_method_types(title, proto, types)
	if not next(types) then return end
	p('\n%-60s %s', title, 'ctype')
	hr()
	local t = {}
	for name in pairs(types) do
		t[#t+1] = name
	end
	table.sort(t)
	for i,name in ipairs(t) do
		p('%-60s %s', name, method_ctype_string(types[name]))
	end
end

local function inspect_methods(title, methods)
	if not methods then return end
	p('\n%-60s %s', title, 'ctype')
	hr()
	for meth in methods do
		p('%-60s %s', method_name(meth), method_ctype_string(meth))
	end
end

local function protocols_spec(protocols)
	local t = {}
	for i,proto in apairs(protocols) do
		t[#t+1] = protocol_name(proto) .. protocols_spec(protocol_protocols(proto))
	end
	return #t > 0 and _(' <%s>', table.concat(t, ', ')) or ''
end

local function class_spec(cls, indent)
	indent = indent or 1
	local super_spec = superclass(cls) and
		_('\n|%s<- %s', ('\t'):rep(indent), class_spec(superclass(cls), indent + 1)) or ''
	return class_name(cls) .. protocols_spec(class_protocols(cls)) .. super_spec
end

local function protocol_spec(proto)
	return protocol_name(proto) .. protocols_spec(protocol_protocols(proto))
end

local function filter(a, patt)
	local t = {}
	for i,e in ipairs(sorta(a)) do
		e = tostring(e)
		if not e:find'^_' and (not patt or e:find(patt)) then
			t[#t+1] = e
		end
	end
	return t
end

local function inspect_names(title, t, f)
	if #t == 0 then return end
	f = f or tostring
	p(title)
	for i,s in ipairs(t) do
		print('', f(s))
	end
end

function inspect.findmethod(cls, patt, methods, t)
	if not methods then
		local t = {}
		inspect.findmethod(cls, patt, instance_methods, t)
		inspect.findmethod(cls, patt, class_methods, t)
		inspect_names(_('Class %s:', tostring(cls)), t, function(meth)
			return _('%-60s %s', tostring(meth), method_ctype_string(method_type(meth)))
		end)
		return
	end
	for i,meth in apairs(methods(class(cls), inst)) do
		local name = tostring(meth)
		if not name:find'^_' and (not patt or name:find(patt)) then
			t[#t+1] = meth
		end
	end
end

function inspect.find(patt)
	--TODO: find framework
	inspect_names('Classes:', filter(classes(), patt))
	inspect_names('Protocols:', filter(protocols(), patt))
	for i,cls in ipairs(sorta(classes())) do
		if not tostring(cls):find'^_' then
			inspect.findmethod(cls, patt)
		end
	end
end

function inspect.protocol(proto)
	proto = protocol(proto)
	header('Protocol %s%s', protocol_name(proto), protocols_spec(protocol_protocols(proto)))
	inspect_properties(protocol_properties(proto))
	inspect_method_types('Instance Methods (required):', proto, protocol_methods(proto, true,  true))
	inspect_method_types('Instance Methods (optional):', proto, protocol_methods(proto, true,  false))
	inspect_method_types('Class Methods (required):',    proto, protocol_methods(proto, false, true))
	inspect_method_types('Class Methods (optional):',    proto, protocol_methods(proto, false, false))
end

function inspect.class_header(cls)
	header('Class %s', class_spec(class(cls)))
end

function inspect.class(cls)
	cls = class(cls)
	inspect.class_header(cls)
	inspect_properties(properties(cls))
	inspect_methods('Instance Methods:', class_methods(cls, true))
	inspect_methods('Class Methods:',    class_methods(cls, false))
end

function inspect.conforms(cls)
	local t = {} --{sel = protocol}
	for i,proto in apairs(class_protocols(cls)) do
		for i,sel in protocol_method_types(proto) do
			t[nptr(sel)] = proto
		end
	end
end

return inspect
