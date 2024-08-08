import {
  DOMWidgetModel,
  DOMWidgetView,
  ISerializers,
  //WidgetModel,
  //WidgetView,
} from '@jupyter-widgets/base';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const widgets = require('@jupyter-widgets/base');

import { MODULE_NAME, MODULE_VERSION } from './version';

// Import the CSS
import '../css/widget.css';

import * as molstar from './base-viewer';
import { ViewSet } from './utils';

export class ViewerModel extends DOMWidgetModel {
  defaults() {
    return {
      ...super.defaults(),

      layout$show_controls: true,

      _model_name: 'ViewerModel',
      _model_module: MODULE_NAME,
      _model_module_version: MODULE_VERSION,
      _view_name: 'ViewerView',
      _view_module: MODULE_NAME,
      _view_module_version: MODULE_VERSION,
    };
  }

  static serializers: ISerializers = {
    components: { deserialize: widgets.unpack_models },
    ...DOMWidgetModel.serializers,
  };
}


export class ViewerView extends DOMWidgetView {
  viewer_container: any;
  viewer_obj: any;
  componentViews: any;
  plugin: any;
  _focused: boolean = false;
  isLeader: boolean = false;
  in_components_changing = false;


  initialize(parameters: any): void {
    super.initialize(parameters);
    this.componentViews = new ViewSet(
      this.create_molstar_child_view,
      this.remove_molstar_child_view,
      this
    );
    this.model.on('msg:custom', this.handle_custom_message.bind(this));
  }

  async render() {
    super.render();
    this.displayed.then(() => {
      this.el.classList.add('molstar-viewer');
      this.el.classList.add('jupyter-widgets');
      this.el.setAttribute('data-jp-suppress-context-menu', '');

      this.viewer_container = document.createElement('div');
      this.el.appendChild(this.viewer_container);

      molstar.Viewer.create(this.viewer_container, {
        layoutIsExpanded: false,
        layoutShowControls: this.model.get('layout$show_controls'),
        layoutShowRemoteState: false,
        layoutShowSequence: true,
        layoutShowLog: true,
        layoutShowLeftPanel: true,

        viewportShowExpand: true,
        viewportShowSelectionMode: false,
        viewportShowAnimation: false,

        pdbProvider: 'rcsb',
        emdbProvider: 'rcsb',
      }).then(viewer => {
        this.viewer_obj = viewer;
        this.viewer_obj.plugin.layout.events.updated.subscribe(() => {
          this.model.set('layout$show_controls', this.viewer_obj.plugin.layout.state.showControls);
          this.model.save_changes();
        });
        this.model.on('change:layout$show_controls', () => {
          this.viewer_obj.setOptions({ layoutShowControls: this.model.get('layout$show_controls') });
        });
      });
    });
  }

  processLuminoMessage(msg: any): void {
    super.processLuminoMessage(msg);
    if ((msg.type === 'resize' || msg.type === 'after-show') && this.viewer_obj) {
      this.viewer_obj.handleResize();
    }
  }

  handle_log_message(content: any): void {
    switch (content.level) {
      case 'info':
        this.viewer_obj.plugin.log.info(content.text);
        break;
      case 'warn':
        this.viewer_obj.plugin.log.warn(content.text);
        break;
      case 'error':
        this.viewer_obj.plugin.log.error(content.text);
        break;
      default:
        this.viewer_obj.plugin.log.message(content.text);
        break;
    }
  }

  handle_custom_message(content: any): void {
    if (this.viewer_obj) {
      switch (content.do) {
        case 'log':
          this.handle_log_message(content);
          break;
        case 'load_pdb':
          this.viewer_obj.loadPdb(content.pdb, content.options);
          break;
        case 'load_structure_from_data':
          this.viewer_obj.loadStructureFromData(content.data, content.format, content.options);
          break;
      }
    }
  }

  create_molstar_child_view(model: any) {
    return this.create_child_view(model, {
      viewer_obj: this.viewer_obj
    });
  }

  remove_molstar_child_view(view: any) {
    view.remove();
  }
}
