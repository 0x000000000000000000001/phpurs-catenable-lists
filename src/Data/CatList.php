<?php

class CatListObj {
    public $array;
    public $offset;

    public function __construct(array $array, int $offset = 0) {
        $this->array = $array;
        $this->offset = $offset;
    }
}

$exports['empty'] = new CatListObj([]);

$exports['null'] = function($cat) {
    return \count($cat->array) === $cat->offset;
};

$exports['length'] = function($cat) {
    return \count($cat->array) - $cat->offset;
};

$exports['append'] = function($cat1) {
    return function($cat2) use ($cat1) {
        $a1 = \array_slice($cat1->array, $cat1->offset);
        $a2 = \array_slice($cat2->array, $cat2->offset);
        return new CatListObj(\array_merge($a1, $a2));
    };
};

$exports['cons'] = function($a) {
    return function($cat) use ($a) {
        if ($cat->offset > 0) {
            $newArr = $cat->array;
            $newArr[$cat->offset - 1] = $a;
            return new CatListObj($newArr, $cat->offset - 1);
        }
        $a1 = \array_slice($cat->array, $cat->offset);
        array_unshift($a1, $a);
        return new CatListObj($a1);
    };
};

$exports['singleton'] = function($a) {
    return new CatListObj([$a]);
};

$exports['snoc'] = function($cat) {
    return function($a) use ($cat) {
        $newArr = $cat->array;
        $newArr[] = $a;
        return new CatListObj($newArr, $cat->offset);
    };
};

$exports['unconsImpl'] = function($nothing) {
    return function($just) use ($nothing) {
        return function($cat) use ($nothing, $just) {
            if (\count($cat->array) === $cat->offset) {
                return $nothing;
            }
            $val = $cat->array[$cat->offset];
            $rest = new CatListObj($cat->array, $cat->offset + 1);
            $f = $just($val);
            return $f($rest);
        };
    };
};

return $exports;
