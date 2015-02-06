JSON Relay для [Skuapso]
========================

Модуль для программы получения данных с навигационных терминалов [Skuapso].

Модуль отправляет пакеты с координатами в формате JSON POST-запросом на указанный в настройках URL.

_**Внимание!** Модуль находится на ранней стадии разработки._


Установка
---------

 1. Добавьте следующую строку в файл `rebar.config` в каталоге с [Skuapso]:

    ```erlang
    ,{json_relay, ".*", {git, "https://github.com/Envek/skuapso_json_relay.git", "HEAD"}}
    ```

 2. Выполните `make deps && make`

 3. В файл `etc/skuapso.config` добавьте `json_relay` в список `modules` и добавьте следующую конструкцию с настройками в этот же файл:

    ```erlang
    {json_relay,[{weight,5},{url, "http://example.com:port/path"}]},
    ```

 4. Запускайте [Skuapso] как обычно.


Скучная юридическая фигня
-------------------------

Данное программное обеспечение распространяется на условиях лицензии [MIT].


[Skuapso]: https://github.com/skuapso/skuapso
[MIT]:     http://opensource.org/licenses/MIT
