local obj = {}
obj.__index = obj
obj.__name = "seal_myactions"
obj.icon = hs.image.imageFromName(hs.image.systesmImageNames.ActionTemplate)

obj.actions ={}


function obj:commands()
  return {}
end

function objare()
  return self.bareActions
end

function obj.bareAction(query)
  local choices = {}
  -- if user has not entered anything, return no choices
  if query == nil or query == ""  then
    return choices
  end

  for action, v in pairs(obj.actions) do
    if string.match(action:lower(), query:lower()) then
      local kind
      if type(v) == 'table' then
        if v.fn then
          kind = 'runFunction'
        elseif v.url then
          kind = 'openURL'
        end
        if kind then
          local choice = {}
          choice.text = action
          choice.type = kind
          choice.plugin = obj.__name
          choice.image = v.icon or obj.icon
          table.insert(choices, choice)
        end
      end
    end
  end

  return choices
end
