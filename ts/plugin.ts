import {
  ILayoutRestorer,
  JupyterFrontEnd,
  JupyterFrontEndPlugin
} from '@jupyterlab/application';

import { Token } from '@lumino/coreutils';

import { ICommandPalette, IWidgetTracker, WidgetTracker } from '@jupyterlab/apputils';

import { DocumentRegistry, IDocumentWidget } from '@jupyterlab/docregistry';

import {
  MolViewer,
  MolViewerFactory
} from './mol-viewer';

import { IJupyterWidgetRegistry } from '@jupyter-widgets/base';

import { MODULE_NAME, MODULE_VERSION } from './version';

const EXTENSION_ID = 'molstar-clj:plugin';

export interface IMolstarPlugin
  extends IWidgetTracker<IDocumentWidget<MolViewer>> { }

export const molstarToken = new Token<IMolstarPlugin>(EXTENSION_ID);

const FILE_TYPES = [{
  displayName: 'Crystallographic Information File',
  name: 'cif',
  mimeTypes: ['chemical/x-cif'],
  extensions: ['.cif']
}, {
  displayName: 'Macromolecular Crystallographic Information File',
  name: 'mmcif',
  mimeTypes: ['chemical/x-mmcif'],
  extensions: ['.mmcif']
}, {
  displayName: 'Tripos Mol2',
  name: 'mol2',
  extensions: ['.mol2']
}, {
  displayName: 'Protein Data Bank File',
  name: 'pdb',
  mimeTypes: ['chemical/x-pdb'],
  extensions: ['.pdb', '.pqr', '.ent']
}, {
  displayName: 'MDL Molfile',
  name: 'mol',
  mimeTypes: ['chemical/x-mdl-molfile'],
  extensions: ['.mol']
}, {
  displayName: 'Structure Data File',
  name: 'sdf',
  mimeTypes: ['chemical/x-mdl-sdfile'],
  extensions: ['.sdf', '.sd']
}]

const FACTORY = 'Molecule Viewer';

const molstarPlugin: JupyterFrontEndPlugin<IMolstarPlugin> = {
  id: EXTENSION_ID,
  provides: molstarToken,
  requires: [IJupyterWidgetRegistry as any],
  optional: [ICommandPalette, ILayoutRestorer],
  activate: activateWidgetExtension,
  autoStart: true,
};

export default molstarPlugin;

/**
 * Activate the widget extension.
 */
function activateWidgetExtension(
  app: JupyterFrontEnd,
  registry: IJupyterWidgetRegistry,
  palette: ICommandPalette | null,
  restorer: ILayoutRestorer | null
): IMolstarPlugin {
  const namespace = 'molstar-clj';

  registry.registerWidget({
    name: MODULE_NAME,
    version: MODULE_VERSION,

    exports: async () => {
      return {
        ...await import('./viewer')
      }
    }
  });

  function onWidgetCreated(
    sender: any,
    widget: IDocumentWidget<MolViewer, DocumentRegistry.IModel>
  ) {
    widget.context.pathChanged.connect(() => {
      void tracker.save(widget);
    });
    void tracker.add(widget);

    const types = app.docRegistry.getFileTypesForPath(widget.context.path);

    if (types.length > 0) {
      widget.title.icon = types[0].icon!;
      widget.title.iconClass = types[0].iconClass ?? '';
      widget.title.iconLabel = types[0].iconLabel ?? '';
    }
  }

  FILE_TYPES.forEach(type => app.docRegistry.addFileType(type))
  const FILE_TYPE_NAMES = FILE_TYPES.map(type => type.name)

  const factory = new MolViewerFactory({
    name: FACTORY,
    fileTypes: FILE_TYPE_NAMES,
    defaultFor: FILE_TYPE_NAMES,
    readOnly: true
  });

  app.docRegistry.addWidgetFactory(factory);
  factory.widgetCreated.connect(onWidgetCreated);

  const tracker = new WidgetTracker<IDocumentWidget<MolViewer>>({
    namespace
  });

  if (restorer) {
    void restorer.restore(tracker, {
      command: 'docmanager:open',
      args: widget => ({
        path: widget.context.path,
        factory: FACTORY
      }),
      name: widget => widget.context.path
    });
  }

  return tracker;
}
