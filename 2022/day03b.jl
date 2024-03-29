function priority(item)
    ascii = convert(Int, item)
    return ascii - (ascii < 97 ? 38 : 96)
end

function main()
    rucksacks = readlines(open("input.txt"))
    total = 0
    
    for group in eachcol(reshape(rucksacks, 3, :))
        total += priority(first(intersect(group...)))
    end

    println(total)
end

main()
