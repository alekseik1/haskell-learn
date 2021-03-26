-- Определите какие-нибудь (разумные) выражения типов:
-- a) [([(Int,Int -> Int)], Char)];
-- Питоноподобный пример:
-- [
--  ([f(x, y) -> Int], 'a'),
--  ([g(x, y) -> Int], 'b'),
--  ...
-- ]
-- Даны функции мнения преподавателя: (формальная оценка -> Финальная оценка), причем об i-ом студенте свое мнение. Студенты группируются по группам, группы именуются по буквам, студенты в них нумеруются числами
good_student :: Int -> Int
good_student  x = x + 2
typical_student :: Int -> Int
typical_student x = x
hate_him :: Int -> Int
hate_him x = x - 3

array :: [([(Int, Int -> Int)], Char)]
array = [
    ([(0, good_student), (1, hate_him), (3, typical_student)], 'a'),
    ([(100, hate_him), (1233, good_student), (321, typical_student)], 'b')
    ]
-- b) [(Char, Int)] -> ([Int], [Char])
-- Дан массив пар ('буква группы', id_студента), надо получить массив всех студентов и массив всех групп
add_value :: ([Int], [Char]) -> (Char, Int) -> ([Int], [Char])
add_value (num_lst_old, chr_list_old) (chr, num) = (num_lst_old ++ [num], chr_list_old ++ [chr])
summary :: [(Char, Int)] -> ([Int], [Char])
summary x = foldl add_value ([], []) x
-- c) a -> (a, a)
-- По данному элементу вернуть кортеж с ним и с ним
duplicate :: a -> (a, a)
duplicate a = (a, a)
-- d) (a -> b) -> a -> b
func1 :: (a -> b) -> a -> b
helper x = [x] ++ [x]
func1 helper_func arg = helper_func arg

main = print "ok"



