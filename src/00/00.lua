local function parse_input(lines)
    local depths = {}
    for line in lines do
        table.insert(depths, tonumber(line))
    end
    return depths
end

local function sliding_window_sum(values, n)
    local queue = {}
    local results = {}
    local sum = 0
    for i=1, #values do
        sum = sum + values[i]
        table.insert(queue, values[i])
        if #queue == n then
            table.insert(results, sum)
            sum = sum - table.remove(queue, 1)
        end
    end
    return results
end

local function count_larger(values)
    local count = 0
    local last_depth = nil
    for _, depth in ipairs(values) do
        if last_depth ~= nil and depth > last_depth then
            count = count + 1
        end
        last_depth = depth
    end
    return count
end

local depths = parse_input(io.open("input.txt"):lines())
print(count_larger(depths))
print(count_larger(sliding_window_sum(depths, 3)))
