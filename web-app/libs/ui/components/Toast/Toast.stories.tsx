import type { Meta, StoryObj } from '@storybook/react';
import { Toast } from './Toast';
import { allowedIconNames } from '../../icons/names';

const meta: Meta<typeof Toast> = {
  title: 'Components/Toast',
  parameters: {
    docs: {
      description: {
        component: 'Props with the item prefix are of the Toast.Item subcomponent'
      }
    }
  },
  component: Toast,
  argTypes: {
    removeTimer: {
      type: 'number',
      control: {
        type: 'number'
      },
      description:
        'Sets a time for each toast to be removed, can be set to false so the items remain (created mostly for the stories)'
    },
    size: {
      control: { type: 'radio' },
      options: ['sm', 'md', 'lg', 'full'],
      description: 'Wrapper size'
    }
  }
};

export default meta;
type Story = StoryObj<typeof meta>;

export const Default: Story = {
  args: {
    itemIcon: 'check',
    itemContent: 'Sample message',
    itemVariant: 'default',
    itemTint: false,
    itemDescription: '',
    itemProgressBar: 0,
    itemActions: undefined
  },
  render: (args) => (
    <Toast removeTimer={false}>
      <Toast.Item
        icon={args.itemIcon}
        tint={args.itemTint}
        progressBar={args.itemProgressBar}
        description={args.itemDescription}
        variant={args.itemVariant}
        actions={args.itemActions}
      >
        {args.itemContent}
      </Toast.Item>
    </Toast>
  )
};
Default.argTypes = {
  itemIcon: {
    control: {
      type: 'select'
    },
    options: [...allowedIconNames]
  },
  itemContent: {
    control: {
      type: 'text'
    }
  },
  itemDescription: {
    description: 'Extra separated content'
  },
  itemProgressBar: {
    description: 'A progress bar on the bottom of the toast',
    control: { type: 'range' }
  },
  itemVariant: {
    control: {
      type: 'select'
    },
    options: ['default', 'success', 'error', 'warning']
  },
  itemTint: {
    description: 'Gives the toast a tint based on its variant, with Default this does nothing'
  },
  itemActions: {
    description: 'Actions which can do something'
  }
};

export const Tinted: Story = {
  render: () => (
    <Toast removeTimer={false}>
      <Toast.Item variant="success" tint>
        Sample text
      </Toast.Item>
    </Toast>
  )
};

export const Progress: Story = {
  render: () => (
    <Toast removeTimer={false}>
      <Toast.Item icon="triangle-exclamation" progressBar={70} variant="error">
        Sample text
      </Toast.Item>
    </Toast>
  )
};

export const Description: Story = {
  render: () => (
    <Toast removeTimer={false}>
      <Toast.Item icon="bolt" description="Describing something...">
        Sample text
      </Toast.Item>
    </Toast>
  )
};

export const Actions: Story = {
  render: () => (
    <Toast removeTimer={false}>
      <Toast.Item
        progressBar={70}
        actions={
          <>
            <button className="text-sm font-medium text-blue-600">Dismiss</button>
            <button className="text-sm font-medium text-link-base">Learn more</button>
          </>
        }
      >
        Sample
      </Toast.Item>
    </Toast>
  )
};

export const ExampleRemove: Story = {
  render: () => (
    <>
      <h2 className="m-2">Try removing a toast, these also dissapear after 10s</h2>
      <Toast>
        <Toast.Item
          icon="triangle-exclamation"
          variant="warning"
          tint
          description="Something isnt right..."
        >
          Sample text
        </Toast.Item>
        <Toast.Item
          actions={
            <>
              <button className="text-sm font-medium text-blue-600">Dismiss</button>
              <button className="text-sm font-medium text-link-base">Learn more</button>
            </>
          }
        >
          Sample text
        </Toast.Item>
        <Toast.Item progressBar={50}>Sample text</Toast.Item>
        <Toast.Item variant="success" progressBar={70} icon="bolt">
          Sample text
        </Toast.Item>
      </Toast>
    </>
  )
};
