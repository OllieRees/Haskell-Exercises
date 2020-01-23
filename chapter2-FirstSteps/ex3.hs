-- Original:
--N = a 'div' length xs
    -- where
        --  a = 10
        -- xs = [1, 2, 3, 4, 5]

-- Backquotes not used around div. xs indented differently to a. Function starts with an uppercase - not lowercase
n = div a (length xs)
        where
            a = 10
            xs = [1..5]