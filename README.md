# Yofication with Emacs

_Yofication_ is described in
[English](https://en.wikipedia.org/wiki/Yoficator) and
[Russian](https://ru.wikipedia.org/wiki/Ёфикатор) Wikipedias. This
`yomacs` project provides a means to carry out yofication using GNU
Emacs 24.3 or newer. It is based on the
[E. Minkovskii's yoficator](http://python.anabar.ru/yo.htm) (GPL). The
main assets of `yomacs` are

1. the `yo.t` dictionary (updated version of the Minkovskii's
   dictionary) and
2. Elisp package `yo.el`, which extends the Minkovskii's package with
   `yo-context` function for auromatic yofication of some word
   combinations (like _обо вс**ё**м_) and the _recursive edit_ option
   in the dictionary-based replacement function `yo-spell`.

This software is available on the terms of GNU General Public License
[version 3.0](https://www.gnu.org/licenses/gpl-3.0.en.html) (Free
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA).


# Ёфикация с помощью Эмакса
Проект `yomacs` основан на
[ёфикаторе Евгения Миньковского](http://python.anabar.ru/yo.htm) и
дополняет его в двух отношениях:

1. внесены некоторые правки в словарь `yo.t`;
2. скрипт на Лиспе дополнен функцией контекстной ёфикации `yo-context`
   для словосочетаний типа «обо вс**ё**м», «по н**ё**м», «вс**ё**,
   что» и т.д.; кроме того, в диалоговую функцию словарной замены
   `yo-spell` добавлена возможность выхода в «рекурсивное
   редактирование».

## Установка
Скачайте файлы `yo.el` и `yo.t` --- скажем, в директорию `Ё`. (Можно
поместить их в разные директории --- но тогда в файле `yo.el` надо
прописать путь к `yo.t`.) В файл инциализации Эмакса (обычно,
`.emacs`) добавьте

``` elisp
(autoload 'yo-context "Ё/yomacs/yo.el" "обо всём etc" t)
(autoload 'yo-spell "Ё/yomacs/yo.el" "dict yofication" t)
(autoload 'yo-yo-rm-entry "Ё/yomacs/yo.el" "Исключить слово" t)
(autoload 'yo-yo-rm-many "Ё/yomacs/yo.el" "Исключить список слов" t)
```
(замените `Ё` на правильный путь).

Не используйте формы `load` или `require`: это замедлит запуск Эмакса
за счёт инициализации хэша в пакете `yo.el`, и приведёт к потере
оперативной памяти, занимаемой этим хэшем --- даже в сеансах, которые
не обращаются к ёфикатору.

## Порядок работы
### 1. Ёфикация небольшого текста
При обработке небольшого текста, вроде статьи для Википедии, можно
просто прощёлкать все проблемные слова, используя функцию `yo-spell`,
так же, как при использовании ёфикатора Миньковского:

Откройте ёфицируемый файл и выполните команду
``` emacs
M-x yo-spell
```

Слова в которых буква Е несомненно занимает место буквы Ё (например,
*актер, береза*), заменяются автоматически; остальные ---
интерактивно, о каждом вхождении сомнительного слова в минибуфере
появляется вопрос. Например:

    Меняем "запрет" на "запрёт"? (Да={SPC|y}, Нет={DEL|n})

Пробел, `y`, `Y` замену подтверждают; `DEL`, `Backspace`, `n`, `N`
замену запрещают. Заглавные буквы `Y` и `N` кроме того
приостанавливают ёфикацию (см. ниже, п.??).

### 2. Контекстная замена
Пожалуй, самая частая проблемная пара --- это местоимения `все/всё`.
Нередко --- по нескольку вхождений на странице, Сотни страниц ---
сотни запросов про `все/всё`.

устойчивые сочетания

всём

### 3. Правка словаря ёфикации
Приведённые ниже рекомендации относятся к обработке больших текстов
(например, целая книга *ebook.fb2*). 

### 4. Ёфикация выделенной области

### 5. Вычитка текста

## Лицензия