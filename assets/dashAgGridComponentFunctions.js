var dagcomponentfuncs = window.dashAgGridComponentFunctions = window.dashAgGridComponentFunctions || {};

function buildButton(props, actionKey) {
    var setData = props && props.setData;
    var data = props && props.data;
    var params = props && props.cellRendererParams ? props.cellRendererParams : {};
    var className = props && props.className ? props.className : params.className;
    var tooltip = props && props.tooltip ? props.tooltip : params.tooltip;
    var label = (props && props.value) || (props && props.label) || params.label || '';

    function onClick(event) {
        if (event) {
            event.stopPropagation();
        }
        if (typeof setData !== 'function' || !data) {
            return;
        }
        var next = Object.assign({}, data, {
            __action: actionKey,
            __action_ts: Date.now(),
        });
        setData(next);
    }

    var disabled = !!(props && props.disabled);

    return React.createElement(
        'button',
        {
            onClick: onClick,
            className: className,
            title: tooltip,
            disabled: disabled,
        },
        label
    );
}

dagcomponentfuncs.RunActionButton = function (props) {
    var disabled = props && props.data ? props.data.run_enabled === false : false;
    return buildButton(Object.assign({}, props, { disabled: disabled }), 'run');
};

dagcomponentfuncs.DeleteActionButton = function (props) {
    var disabled = !(props && props.data && props.data.delete_enabled);
    return buildButton(Object.assign({}, props, { disabled: disabled }), 'delete');
};
