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
  }

  async render() {
    super.render();
    this.displayed.then(() => {
      this.el.classList.add('molstar-stage');
      this.el.classList.add('jupyter-widgets');
      this.el.setAttribute('data-jp-suppress-context-menu', '');

      this.viewer_container = document.createElement('div');
      this.el.appendChild(this.viewer_container);

      molstar.Viewer.create(this.viewer_container, {
        layoutIsExpanded: true,
        layoutShowControls: false,
        layoutShowRemoteState: false,
        layoutShowSequence: true,
        layoutShowLog: false,
        layoutShowLeftPanel: true,

        viewportShowExpand: true,
        viewportShowSelectionMode: false,
        viewportShowAnimation: false,

        pdbProvider: 'rcsb',
        emdbProvider: 'rcsb',
      }).then(viewer => {
        this.viewer_obj = viewer;
        viewer.loadPdb('7bv2');
        viewer.loadEmdb('EMD-30210', { detail: 6 });
      });
    });
  }

  processLuminoMessage(msg: any): void {
    super.processLuminoMessage(msg);
    /*if ((msg.type === 'resize' || msg.type === 'after-show') && this.viewer_obj) {
      const box = this.el.getBoundingClientRect();
      this.viewer_obj.setSize(Math.floor(box.width) + 'px', Math.floor(box.height) + 'px');
      }*/
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
