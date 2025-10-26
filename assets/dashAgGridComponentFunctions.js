var dagcomponentfuncs = window.dashAgGridComponentFunctions = window.dashAgGridComponentFunctions || {};

function buildButton(props, actionKey) {
    var setData = props && props.setData;
    var data = props && props.data;
    var node = props && props.node;
    var params = props && props.cellRendererParams ? props.cellRendererParams : {};
    var className = props && props.className ? props.className : params.className;
    var tooltip = props && props.tooltip ? props.tooltip : params.tooltip;
    var label = (props && props.value) || (props && props.label) || params.label || '';
    var style = Object.assign({}, params.style || {}, props && props.style ? props.style : {});
    if (!style.width) {
        style.width = '120px';
    }

    var setProps = null;
    if (props) {
        if (typeof props.setProps === 'function') {
            setProps = props.setProps;
        } else if (props.context && typeof props.context.setProps === 'function') {
            setProps = props.context.setProps;
        } else if (
            props.agGridReact &&
            props.agGridReact.props &&
            typeof props.agGridReact.props.setProps === 'function'
        ) {
            setProps = props.agGridReact.props.setProps;
        }
    }

    function onClick(event) {
        if (event) {
            event.stopPropagation();
        }
        if (!data) {
            return;
        }
        var timestamp = Date.now();
        var next = Object.assign({}, data, {
            __action: actionKey,
            __action_ts: timestamp,
        });
        if (typeof setData === 'function') {
            setData(next);
        } else if (node && typeof node.setData === 'function') {
            node.setData(next);
        } else if (props && props.api && typeof props.api.applyTransaction === 'function') {
            props.api.applyTransaction({ update: [next] });
        }
        if (setProps) {
            var payload = {
                action: actionKey,
                timestamp: timestamp,
                code: next.code || next.method || null,
                data: next,
            };
            try {
                setProps({ cellRendererData: payload });
            } catch (err) {
                if (typeof console !== 'undefined' && console && typeof console.warn === 'function') {
                    console.warn('Failed to send cellRendererData', err);
                }
            }
        }
    }

    var disabled = !!(props && props.disabled);

    return React.createElement(
        'button',
        {
            onClick: onClick,
            className: className,
            title: tooltip,
            disabled: disabled,
            style: style,
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
