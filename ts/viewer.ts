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
import { ViewSet, resolve_buffer } from './utils';

export class ViewerModel extends DOMWidgetModel {
  defaults() {
    return {
      ...super.defaults(),

      layout$is_expanded: false,
      layout$show_controls: true,
      layout$left_panel: 'full',
      layout$right_panel: 'full',
      layout$top_panel: 'full',
      layout$bottom_panel: 'full',

      viewport$show_expand: true,
      viewport$show_controls: true,
      viewport$show_settings: true,
      viewport$show_selection_mode: true,
      viewport$show_animation: true,
      viewport$show_trajectory_controls: true,

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
        layoutIsExpanded: this.model.get('layout$is_expanded'),
        layoutShowControls: this.model.get('layout$show_controls'),
        layoutLeftPanel: this.model.get('layout$left_panel'),
        layoutRightPanel: this.model.get('layout$right_panel'),
        layoutTopPanel: this.model.get('layout$top_panel'),
        layoutBottomPanel: this.model.get('layout$bottom_panel'),
        layoutShowRemoteState: true,
        layoutShowSequence: true,
        layoutShowLog: true,
        layoutShowLeftPanel: true,

        viewportShowExpand: this.model.get('viewport$show_expand'),
        viewportShowSelectionMode: true,
        viewportShowAnimation: true,
        viewportShowTrajectoryControls: true,

        pdbProvider: 'rcsb',
        emdbProvider: 'rcsb',
      }).then(viewer => {
        this.viewer_obj = viewer;
        this.viewer_obj.plugin.layout.events.updated.subscribe(() => {
          this.model.set('layout$show_controls', this.viewer_obj.plugin.layout.state.showControls);
          this.model.set('layout$is_expanded', this.viewer_obj.plugin.layout.state.isExpanded);
          this.model.set('layout$left_panel', this.viewer_obj.plugin.layout.state.regionState.left);
          this.model.set('layout$right_panel', this.viewer_obj.plugin.layout.state.regionState.right);
          this.model.set('layout$top_panel', this.viewer_obj.plugin.layout.state.regionState.top);
          this.model.set('layout$bottom_panel', this.viewer_obj.plugin.layout.state.regionState.bottom);
          this.model.save_changes();
        });
        this.model.on('change:layout$show_controls', () => {
          this.viewer_obj.setOptions({ layoutShowControls: this.model.get('layout$show_controls') });
        });
        this.model.on('change:layout$is_expanded', () => {
          this.viewer_obj.setOptions({ layoutIsExpanded: this.model.get('layout$is_expanded') });
        });
        this.model.on('change:layout$left_panel', () => {
          this.viewer_obj.setOptions({ layoutLeftPanel: this.model.get('layout$left_panel') });
        });
        this.model.on('change:layout$right_panel', () => {
          this.viewer_obj.setOptions({ layoutRightPanel: this.model.get('layout$right_panel') });
        });
        this.model.on('change:layout$top_panel', () => {
          this.viewer_obj.setOptions({ layoutTopPanel: this.model.get('layout$top_panel') });
        });
        this.model.on('change:layout$bottom_panel', () => {
          this.viewer_obj.setOptions({ layoutBottomPanel: this.model.get('layout$bottom_panel') });
        });
        this.model.on('change:viewport$show_expand', () => {
          this.viewer_obj.setOptions({ viewportShowExpand: this.model.get('viewport$show_expand') });
        });
        console.log(this.viewer_obj);
      });
    });
  }

  processLuminoMessage(msg: any): void {
    super.processLuminoMessage(msg);
    if ((msg.type === 'resize' || msg.type === 'after-show') && this.viewer_obj) {
      this.viewer_obj.handleResize();
    }
  }

  handle_log_message(msg: any): void {
    switch (msg.level) {
      case 'info':
        this.viewer_obj.plugin.log.info(msg.text);
        break;
      case 'warn':
        this.viewer_obj.plugin.log.warn(msg.text);
        break;
      case 'error':
        this.viewer_obj.plugin.log.error(msg.text);
        break;
      default:
        this.viewer_obj.plugin.log.message(msg.text);
        break;
    }
  }

  handle_custom_message(msg: any, buffers: DataView[]): void {
    if (this.viewer_obj) {
      switch (msg.name) {
        case 'log':
          this.handle_log_message(msg);
          break;
        case 'load_pdb':
          this.viewer_obj.loadPdb(msg.pdb, msg.options);
          break;
        case 'load_structure_from_data':
          this.viewer_obj.loadStructureFromData(resolve_buffer(msg.data, msg.isBinary, buffers),
                                                msg.format, msg.options);
          break;
        /*case 'load_trajectory':
          inject_buffer(content.params.model, 'data', content.params.model.isBinary, buffers);
          inject_buffer(content.params.coordinates, 'data', content.params.coordinates.isBinary, buffers);
          this.viewer_obj.loadTrajectory(
          break;*/
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
