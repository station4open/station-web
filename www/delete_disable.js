'use strict';

(function (run) {
	if (document.readyState === 'loading')
		return document.addEventListener('DOMContentLoaded', run);
	else
		return run();
}(function () {
	var forms = document.getElementsByTagName('form');
	for (var i=0; i<forms.length; ++i)
		(function (form) {
			var checkbox = form.querySelector('input[type="checkbox"][name="delete"]');
			var textboxes = form.querySelectorAll('input[type="text"], textarea');
			if (checkbox !== null) {
				checkbox.addEventListener(
					'input',
					function () {
						for (var j=0; j<textboxes.length; ++j)
							if (!textboxes.item(j).readOnly)
								textboxes.item(j).disabled = this.checked;
					}
				)
			}
		})(forms.item(i));
}))
