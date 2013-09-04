Selenium, Python, TinyMCE
#########################

:date: 2012-04-10 16:10
:tags: selenium, tools, javascript


КОСТЫЛИ-КОСТЫЛИКИ

чтобы записать текст в TinyMCE редактор, нужно найти его фрейм, перейти в него, выполнив js код записать во внутрь элемента текст, перейти обратно в главный фрейм и только тогда продолжать работу

.. code-block:: python

    lead, body = browser.find_elements_by_tag_name('iframe')
    browser.switch_to_frame(lead)
    browser.execute_script('document.getElementsByTagName("p")[0].innerHTML="test lead";')
    browser.switch_to_default_content()
    browser.switch_to_frame(body)
    browser.execute_script('document.getElementsByTagName("p")[0].innerHTML="test body";')
    browser.switch_to_default_content()
